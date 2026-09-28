{-# LANGUAGE BangPatterns #-}

{- |
Module      : Data.ASN1.BinaryEncoding.Parse
License     : BSD-style
Copyright   : (c) 2010-2013 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

Generic parsing facility for ASN.1.

The structure of a stream (headers, where primitive contents are and where
constructions end) is read by a single state machine, 'foldrStream', which
works directly on a strict bytestring. It is used twice for every stream:
first to check the whole stream is well-formed, which is cheap and gives
the result its 'Left' or 'Right', and then to produce the events or values
lazily, as they are asked for.
-}

module Data.ASN1.BinaryEncoding.Parse
  ( -- * simple parsing interfaces
    parseLBS
  , parseBS
    -- * the parser the decoders are built on
  , foldrStream
  , checkStream
  ) where

import qualified Control.Exception as E
import           Data.ASN1.Error ( ASN1Error (..) )
import           Data.ASN1.Types.Lowlevel
                   ( ASN1Class (..), ASN1Event (..), ASN1Header (..)
                   , ASN1Length (..), ASN1Tag
                   )
import           Data.Bits ( (.&.), clearBit, shiftL, shiftR, testBit )
import           Data.ByteString ( ByteString )
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as B
import           Data.Word ( Word64, Word8 )

-- | The constructions being read, innermost first, each with its header
-- and, if its length is definite, the stream position where it ends.
data Stack
  = Bottom
  | Definite {-# UNPACK #-} !Word64 !ASN1Header !Stack
  | Indefinite !ASN1Header !Stack

-- | Read the structure of a stream, as a right fold over what is found in
-- it, in stream order.
--
-- This reads streams the way this library always has, including how it
-- treats malformed ones:
--
-- * An end-of-contents header (universal class, tag 0, whatever its length
--   says) ends the innermost construction if its length is indefinite, is an
--   error if it is definite, and is ignored outside of any construction.
--
-- * After every value, the constructions of definite length that end there
--   end. One that has been read past is an error.
--
-- * A construction can end between the header of a primitive and its
--   contents: when the header ends at the position where a construction
--   ends. The primitive header, the ends and then the contents are reported
--   in that order (they are then not a valid sequence of events).
--
-- * A long form length is accumulated in an 'Int', which wraps around. A
--   primitive whose length ends up at or beyond 2^63 has no contents, but
--   still needs input after its header, like any primitive of non-zero
--   length.
foldrStream ::
     (ASN1Header -> r -> r)
     -- ^ The header of a constructed value (its contents follow).
  -> (ASN1Header -> ByteString -> r -> r)
     -- ^ A primitive value: its header and contents.
  -> (ASN1Header -> r -> r)
     -- ^ The header of a primitive value that constructions end after (see
     -- above). Those ends, and then its contents, follow.
  -> (ByteString -> r -> r)
     -- ^ The contents of such a primitive value.
  -> (ASN1Header -> r -> r)
     -- ^ The end of a construction, given the header it started with.
  -> (ASN1Error -> r)
     -- ^ The stream is malformed.
  -> r
     -- ^ The end of the stream.
  -> ByteString
  -> r
foldrStream onCons onPrim onPrimHeader onPrimBody onEnd onError onDone bs =
  go 0 Bottom
 where
  !n = B.length bs

  byte :: Int -> Word8
  byte = B.unsafeIndex bs

  w64 :: Int -> Word64
  w64 = fromIntegral

  -- A header is expected at position i.
  go !i !stack
    | i >= n = case stack of
        Bottom -> onDone
        _      -> onError ParsingPartial
    | otherwise =
        let !w  = byte i
            !pc = testBit w 5
            !cl = classOf w
            !t  = fromIntegral (w .&. 0x1f)
        in  if t == 0x1f
              then longTag cl pc (i + 1) stack
              else lengthOf cl pc t (i + 1) stack

  -- The first byte of a tag in long form.
  longTag !cl !pc !i !stack
    | i >= n = onError ParsingPartial
    | t == 0x80 = onError $ ParsingHeaderFail $
        show (w64 (i + 1)) ++ ":Failed reading: non canonical encoding of long tag"
    | testBit t 7 = longTagRest cl pc (i + 1) (clearBit t 7) stack
    | otherwise = lengthOf cl pc t (i + 1) stack
   where
    t = fromIntegral (byte i) :: ASN1Tag

  longTagRest !cl !pc !i !acc !stack
    | i >= n = onError ParsingPartial
    | testBit t 7 = longTagRest cl pc (i + 1) ((acc `shiftL` 7) + clearBit t 7) stack
    | otherwise = lengthOf cl pc ((acc `shiftL` 7) + t) (i + 1) stack
   where
    t = fromIntegral (byte i) :: ASN1Tag

  lengthOf !cl !pc !tag !i !stack
    | i >= n = onError ParsingPartial
    | not (testBit l 7) = value cl pc tag (LenShort l) (i + 1) stack
    | otherwise = case clearBit l 7 of
        0 -> value cl pc tag LenIndefinite (i + 1) stack
        k | k > n - (i + 1) -> onError ParsingPartial
          | otherwise ->
              value cl pc tag (LenLong k (lengthValue (i + 1) (i + 1 + k) 0)) (i + 1 + k) stack
   where
    l = fromIntegral (byte i) :: Int

  lengthValue :: Int -> Int -> Int -> Int
  lengthValue !i !end !acc
    | i >= end  = acc
    | otherwise = lengthValue (i + 1) end ((acc `shiftL` 8) + fromIntegral (byte i))

  -- A header has been read, j is the position after it.
  value !cl !pc !tag !len !j !stack
    | cl == Universal && tag == 0 = case stack of
        Bottom            -> settle j Bottom
        Definite {}       -> onError StreamUnexpectedEOC
        Indefinite h rest -> onEnd h (settle j rest)
    | pc =
        let !hdr = ASN1Header cl tag True len
            !stack' = case len of
              LenIndefinite -> Indefinite hdr stack
              LenShort l    -> Definite (w64 j + fromIntegral l) hdr stack
              LenLong _ l   -> Definite (w64 j + fromIntegral l) hdr stack
        in  onCons hdr (settle j stack')
    | otherwise =
        let !hdr = ASN1Header cl tag False len
        in  case len of
              LenIndefinite -> onError StreamInfinitePrimitive
              LenShort l    -> primitive hdr (fromIntegral l) j stack
              LenLong _ l   -> primitive hdr (fromIntegral l) j stack

  primitive !hdr !plen !j !stack
    | plen == 0 = onPrim hdr B.empty (settle j stack)
    | otherwise = case stack of
        Definite end _ _
          | w64 j > end  -> onError StreamConstructionWrongSize
          | w64 j == end -> onPrimHeader hdr (endsThenContents plen j stack)
        _ -> contents (onPrim hdr) plen j stack

  endsThenContents !plen !j !stack = case stack of
    Definite end h rest
      | w64 j == end -> onEnd h (endsThenContents plen j rest)
      | w64 j > end  -> onError StreamConstructionWrongSize
    _ -> contents onPrimBody plen j stack

  -- The contents are only read if there is something left to read (even if,
  -- with a length at or beyond 2^63, nothing is read).
  contents emit !plen !j !stack
    | j >= n     = onError ParsingPartial
    | m <= 0     = emit B.empty (settle j stack)
    | m > n - j  = onError ParsingPartial
    | otherwise  = emit (B.unsafeTake m (B.unsafeDrop j bs)) (settle (j + m) stack)
   where
    m = fromIntegral (plen :: Word64) :: Int
  {-# INLINE contents #-}

  -- End the constructions that end at position j, then read on.
  settle !j !stack = case stack of
    Definite end h rest
      | w64 j == end -> onEnd h (settle j rest)
      | w64 j > end  -> onError StreamConstructionWrongSize
    _ -> go j stack
{-# INLINE foldrStream #-}

classOf :: Word8 -> ASN1Class
classOf w = case w `shiftR` 6 of
  0 -> Universal
  1 -> Application
  2 -> Context
  _ -> Private
{-# INLINE classOf #-}

-- | Check the structure of a stream: 'Nothing' if it is well-formed.
checkStream :: ByteString -> Maybe ASN1Error
checkStream =
  foldrStream (\_ k -> k) (\_ _ k -> k) (\_ k -> k) (\_ k -> k) (\_ k -> k)
    Just Nothing

-- | The events of a stream known to be well-formed.
events :: ByteString -> [ASN1Event]
events = foldrStream
  (\h k -> Header h : ConstructionBegin : k)
  (\h p k -> Header h : Primitive p : k)
  (\h k -> Header h : k)
  (\p k -> Primitive p : k)
  (\_ k -> ConstructionEnd : k)
  E.throw
  []

-- | Parse one lazy bytestring and returns on success all ASN.1 events
-- associated.
parseLBS :: L.ByteString -> Either ASN1Error [ASN1Event]
parseLBS = parseBS . L.toStrict

-- | Parse one strict bytestring and returns on success all ASN.1 events
-- associated.
parseBS :: ByteString -> Either ASN1Error [ASN1Event]
parseBS bs = case checkStream bs of
  Just err -> Left err
  Nothing  -> Right (events bs)
