{-# LANGUAGE BangPatterns #-}

{- |
Module      : Data.ASN1.BinaryEncoding
License     : BSD-style
Copyright   : (c) 2010-2013 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

A module containing ASN.1 BER and DER specification encoding/decoding.
-}

module Data.ASN1.BinaryEncoding
  ( BER (..)
  , DER (..)
  ) where

import qualified Control.Exception as E
import           Data.ASN1.BinaryEncoding.Parse ( checkStream, foldrStream )
import           Data.ASN1.Encoding
                   ( ASN1Decoding (..), ASN1DecodingRepr (..), ASN1Encoding (..)
                   )
import           Data.ASN1.Error ( ASN1Error (..) )
import           Data.ASN1.Prim
                   ( decodePrimitive, encodeHeader, encodePrimitive
                   , mkSmallestLength
                   )
import           Data.ASN1.Serialize ( headerLength, pokeHeader )
import           Data.ASN1.Stream ( ASN1Repr )
import           Data.ASN1.Types ( ASN1 (..), ASN1ConstructionType (..) )
import           Data.ASN1.Types.Lowlevel
                   ( ASN1Class (..), ASN1Event (..), ASN1Header (..)
                   , ASN1Length (..)
                   )
import           Data.ByteString ( ByteString )
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B ( unsafeCreate )
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as B
import           Data.Word ( Word8 )
import           Foreign.Marshal.Utils ( copyBytes )
import           Foreign.Ptr ( Ptr, castPtr, plusPtr )

-- | Basic Encoding Rules (BER).
data BER = BER

-- | Distinguished Encoding Rules (DER).
data DER = DER

instance ASN1DecodingRepr BER where
  decodeASN1Repr _ = decodeReprBER . L.toStrict

instance ASN1Decoding BER where
  decodeASN1 _ = decodeBER . L.toStrict

instance ASN1DecodingRepr DER where
  decodeASN1Repr _ = decodeReprDER . L.toStrict

instance ASN1Decoding DER where
  decodeASN1 _ = decodeDER . L.toStrict

instance ASN1Encoding DER where
  encodeASN1 _ = L.fromChunks . encodeValues

decodeBER, decodeDER :: ByteString -> Either ASN1Error [ASN1]
decodeBER = decodeValues (const Nothing)
decodeDER = decodeValues checkDER

decodeReprBER, decodeReprDER :: ByteString -> Either ASN1Error [ASN1Repr]
decodeReprBER = decodeRepr (const Nothing)
decodeReprDER = decodeRepr checkDER

-- | The values of a stream: 'Left' if the stream is malformed, and otherwise
-- the values, produced as they are asked for. A value that cannot be decoded
-- (or whose header the check rejects) is thrown as an exception in place of
-- the rest of the list.
decodeValues ::
     (ASN1Header -> Maybe ASN1Error)
  -> ByteString
  -> Either ASN1Error [ASN1]
decodeValues check bs = case checkStream bs of
  Just err -> Left err
  Nothing  -> Right $
    foldrStream onCons onPrim unexpectedPrimitive (\_ k -> k) onEnd E.throw [] bs
 where
  onCons hdr k = case check hdr of
    Nothing  -> Start (decodeConstruction hdr) : k
    Just err -> E.throw err
  onPrim hdr p k = case check hdr of
    Nothing -> case decodePrimitive hdr p of
      Right obj -> obj : k
      Left err  -> E.throw err
    Just err -> E.throw err
  onEnd hdr k = End (decodeConstruction hdr) : k
{-# INLINE decodeValues #-}

-- | As 'decodeValues', with each value paired with the events it is made of.
decodeRepr ::
     (ASN1Header -> Maybe ASN1Error)
  -> ByteString
  -> Either ASN1Error [ASN1Repr]
decodeRepr check bs = case checkStream bs of
  Just err -> Left err
  Nothing  -> Right $
    foldrStream onCons onPrim unexpectedPrimitive (\_ k -> k) onEnd E.throw [] bs
 where
  onCons hdr k = case check hdr of
    Nothing  -> (Start (decodeConstruction hdr), [Header hdr, ConstructionBegin]) : k
    Just err -> E.throw err
  onPrim hdr p k = case check hdr of
    Nothing -> case decodePrimitive hdr p of
      Right obj -> (obj, [Header hdr, Primitive p]) : k
      Left err  -> E.throw err
    Just err -> E.throw err
  onEnd hdr k = (End (decodeConstruction hdr), [ConstructionEnd]) : k
{-# INLINE decodeRepr #-}

-- | A primitive header followed by the end of a construction, rather than by
-- the primitive's contents, is not a sequence of events there are values
-- for.
unexpectedPrimitive :: ASN1Header -> a -> b
unexpectedPrimitive hdr _ = E.throw $ StreamUnexpectedSituation (show (Header hdr))

decodeConstruction :: ASN1Header -> ASN1ConstructionType
decodeConstruction (ASN1Header Universal 0x10 _ _) = Sequence
decodeConstruction (ASN1Header Universal 0x11 _ _) = Set
decodeConstruction (ASN1Header c t _ _)            = Container c t

-- | DER header need to be all of finite size and of minimum possible size.
checkDER :: ASN1Header -> Maybe ASN1Error
checkDER (ASN1Header _ _ _ len) = checkLength len
 where
  checkLength :: ASN1Length -> Maybe ASN1Error
  checkLength LenIndefinite =
    Just $ PolicyFailed "DER" "indefinite length not allowed"
  checkLength (LenShort _) = Nothing
  checkLength (LenLong n i)
    | n == 1 && i < 0x80  =
        Just $ PolicyFailed "DER" "long length should be a short length"
    | n == 1 && i >= 0x80 = Nothing
    | otherwise = if i >= 2^((n-1)*8) && i < 2^(n*8)
        then Nothing
        else Just $ PolicyFailed "DER" "long length is not shortest"

-- | The encoding of a value, as its size and how to write it.
data Encoding = Encoding !Int (Ptr Word8 -> IO ())

-- | Encode values, a strict chunk for each value at the top level.
--
-- Values are matched up with the 'End' that closes them by counting, and an
-- 'End' inside a construction is otherwise ignored (as is a missing one at
-- the end of the list). An 'End' outside of any construction is an error.
encodeValues :: [ASN1] -> [ByteString]
encodeValues [] = []
encodeValues (s@(Start _) : xs) =
  let (children, rest) = contentsOf xs
  in  render (constructed s children) : encodeValues rest
encodeValues (p : xs) = render (primitive p) : encodeValues xs

-- | The encodings of the values that make up a construction, up to the 'End'
-- that closes it, and the values after that.
contentsOf :: [ASN1] -> ([Encoding], [ASN1])
contentsOf = go []
 where
  go acc []               = (reverse acc, [])
  go acc (End _ : xs)     = (reverse acc, xs)
  go acc (s@(Start _) : xs) =
    let (children, rest) = contentsOf xs
        !e = constructed s children
    in  go (e : acc) rest
  go acc (p : xs)         = let !e = primitive p in go (e : acc) xs

constructed :: ASN1 -> [Encoding] -> Encoding
constructed s children =
  Encoding (hlen + clen) $ \p -> do
    pokeHeader p hdr
    pokeAll (p `plusPtr` hlen) children
 where
  !clen = sum [ size | Encoding size _ <- children ]
  !hdr = encodeHeader True (mkSmallestLength clen) s
  !hlen = headerLength hdr
  pokeAll _ [] = pure ()
  pokeAll q (Encoding size poke : es) = poke q >> pokeAll (q `plusPtr` size) es

primitive :: ASN1 -> Encoding
primitive a = case encodePrimitive a of
  -- The header first: it is what fails for an 'End' (with the same error as
  -- ever), for which there are no contents either.
  (_, Header hdr : rest) -> hdr `seq` case rest of
    [Primitive b] ->
      let !hlen = headerLength hdr
      in  Encoding (hlen + B.length b) $ \p -> do
            pokeHeader p hdr
            B.unsafeUseAsCStringLen b $ \(src, len) ->
              copyBytes (p `plusPtr` hlen) (castPtr src) len
    _ -> error "Data.ASN1.BinaryEncoding: unexpected primitive encoding"
  _ -> error "Data.ASN1.BinaryEncoding: unexpected primitive encoding"

render :: Encoding -> ByteString
render (Encoding size poke) = B.unsafeCreate size poke
