{-# LANGUAGE BangPatterns #-}

{- |
Module      : Data.ASN1.BinaryEncoding.Writer
License     : BSD-style
Copyright   : (c) 2010-2013 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

Serialize events for streaming.
-}

module Data.ASN1.BinaryEncoding.Writer
  ( toByteString
  , toLazyByteString
  ) where

import           Data.ASN1.Serialize ( headerLength, pokeHeader, putHeader )
import           Data.ASN1.Types.Lowlevel
                   ( ASN1Event (..), ASN1Header (..), ASN1Length (..) )
import           Data.ByteString ( ByteString )
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B ( unsafeCreate )
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Unsafe as B
import           Data.Word ( Word8 )
import           Foreign.Marshal.Utils ( copyBytes )
import           Foreign.Ptr ( Ptr, castPtr, plusPtr )

-- | Transform a list of ASN.1 events into a strict bytestring.
--
-- This gives the same bytes as 'toLazyByteString', written straight into
-- one buffer: the headers and the primitive contents, in order. A
-- 'ConstructionEnd' that does not close a constructed 'Header' is an error.
toByteString :: [ASN1Event] -> ByteString
toByteString evs = B.unsafeCreate (size 0 0 evs) (\p -> write p evs)
 where
  size :: Int -> Int -> [ASN1Event] -> Int
  size !acc !_ [] = acc
  size !acc !depth (x:xs) = case x of
    Header hdr@(ASN1Header _ _ pc _) ->
      size (acc + headerLength hdr) (if pc then depth + 1 else depth) xs
    Primitive bs -> size (acc + B.length bs) depth xs
    ConstructionBegin -> size acc depth xs
    ConstructionEnd
      | depth == 0 -> error "malformed stream: end before construction"
      | otherwise  -> size acc (depth - 1) xs

  write :: Ptr Word8 -> [ASN1Event] -> IO ()
  write _ [] = pure ()
  write !p (x:xs) = case x of
    Header hdr -> do
      pokeHeader p hdr
      write (p `plusPtr` headerLength hdr) xs
    Primitive bs -> do
      B.unsafeUseAsCStringLen bs $ \(src, len) -> copyBytes p (castPtr src) len
      write (p `plusPtr` B.length bs) xs
    _ -> write p xs

-- | Transform a list of ASN.1 events into a lazy bytestring.
toLazyByteString :: [ASN1Event] -> L.ByteString
toLazyByteString evs = L.fromChunks $ loop [] evs
 where
  loop _ [] = []
  loop acc (x@(Header (ASN1Header _ _ pc len)):xs) =
    toBs x : loop (if pc then (len == LenIndefinite):acc else acc) xs
  loop acc (ConstructionEnd:xs) = case acc of
    []        -> error "malformed stream: end before construction"
    (True:r)  -> toBs ConstructionEnd : loop r xs
    (False:r) -> loop r xs
  loop acc (x:xs) = toBs x : loop acc xs

  toBs (Header hdr)      = putHeader hdr
  toBs (Primitive bs)    = bs
  toBs ConstructionBegin = B.empty
  toBs ConstructionEnd   = B.empty
