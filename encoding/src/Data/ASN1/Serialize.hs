{-# LANGUAGE BangPatterns #-}

{- |
Module      : Data.ASN1.Serialize
License     : BSD-style
Copyright   : (c) 2010-2013 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown
-}

module Data.ASN1.Serialize
  ( getHeader
  , putHeader
  , headerLength
  , pokeHeader
  ) where

import           Control.Monad ( when )
import           Data.ASN1.Get ( Get, getBytes, getWord8 )
import           Data.ASN1.Internal ( putVarEncodingIntegral )
import           Data.ASN1.Types.Lowlevel
                   ( ASN1Class, ASN1Header (..), ASN1Length (..), ASN1Tag )
import           Data.Bits
                   ( (.&.), (.|.), clearBit, countLeadingZeros, shiftL, shiftR
                   , testBit
                   )
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B ( unsafeCreate )
import           Data.Word ( Word64, Word8 )
import           Foreign.Ptr ( Ptr )
import           Foreign.Storable ( pokeByteOff )

-- | Parse an ASN1 header.
getHeader :: Get ASN1Header
getHeader = do
  (cl, pc, t1) <- parseFirstWord <$> getWord8
  tag <- if t1 == 0x1f then getTagLong else pure t1
  ASN1Header cl tag pc <$> getLength

-- | Parse the first word of an header.
parseFirstWord :: Word8 -> (ASN1Class, Bool, ASN1Tag)
parseFirstWord w = (cl, pc, t1)
 where
  cl = toEnum $ fromIntegral (w `shiftR` 6)
  pc = testBit w 5
  t1 = fromIntegral (w .&. 0x1f)

{- when the first tag is 0x1f, the tag is in long form, where
 - we get bytes while the 7th bit is set. -}
getTagLong :: Get ASN1Tag
getTagLong = do
  t <- fromIntegral <$> getWord8
  when (t == 0x80) $ fail "non canonical encoding of long tag"
  if testBit t 7
    then loop (clearBit t 7)
    else pure t
 where
  loop n = do
    t <- fromIntegral <$> getWord8
    if testBit t 7
      then loop (n `shiftL` 7 + clearBit t 7)
      else pure (n `shiftL` 7 + t)

{- get the asn1 length which is either short form if 7th bit is not set,
 - indefinite form is the 7 bit is set and every other bits clear,
 - or long form otherwise, where the next bytes will represent the length
 -}
getLength :: Get ASN1Length
getLength = do
  l1 <- fromIntegral <$> getWord8
  if testBit l1 7
    then case clearBit l1 7 of
      0   -> pure LenIndefinite
      len -> do
        lw <- getBytes len
        pure (LenLong len $ uintbs lw)
    else
      pure (LenShort l1)
 where
  {- uintbs return the unsigned int represented by the bytes -}
  uintbs = B.foldl (\acc n -> (acc `shiftL` 8) + fromIntegral n) 0

-- | putIdentifier encode an ASN1 Identifier into a marshalled value.
putHeader :: ASN1Header -> B.ByteString
putHeader hdr = B.unsafeCreate (headerLength hdr) (`pokeHeader` hdr)

-- | The number of bytes 'putHeader' gives for a header.
headerLength :: ASN1Header -> Int
headerLength (ASN1Header _ tag _ len) = 1 + tagLength + lengthLength len
 where
  tagLength
    | tag < 0x1f = 0
    | otherwise  = B.length (putVarEncodingIntegral tag)

-- | Write what 'putHeader' gives for a header, which must fit.
pokeHeader :: Ptr Word8 -> ASN1Header -> IO ()
pokeHeader p (ASN1Header cl tag pc len) = do
  pokeByteOff p 0 word1
  off <- if tag < 0x1f
    then pure 1
    else do
      let tagBS = putVarEncodingIntegral tag
      mapM_ (\(i, w) -> pokeByteOff p i w) (zip [1 ..] (B.unpack tagBS))
      pure (1 + B.length tagBS)
  pokeLength p off len
 where
  cli = shiftL (fromIntegral $ fromEnum cl) 6
  pcval = shiftL (if pc then 0x1 else 0x0) 5
  tag0 = if tag < 0x1f then fromIntegral tag else 0x1f
  word1 = cli .|. pcval .|. tag0 :: Word8

-- | The number of bytes a length takes: one, or for the long form one more
-- than the shortest big-endian form of the length.
lengthLength :: ASN1Length -> Int
lengthLength (LenShort i)
  | i < 0 || i > 0x7f = errorShortLength
  | otherwise         = 1
lengthLength (LenLong _ i)
  | i < 0     = errorLongLength
  | otherwise = 1 + uintLength (fromIntegral i)
lengthLength LenIndefinite = 1

-- | Write a length.
pokeLength :: Ptr Word8 -> Int -> ASN1Length -> IO ()
pokeLength p off (LenShort i)
  | i < 0 || i > 0x7f = errorShortLength
  | otherwise         = pokeByteOff p off (fromIntegral i :: Word8)
pokeLength p off (LenLong _ i)
  | i < 0     = errorLongLength
  | otherwise = do
      let w = fromIntegral i :: Word64
          k = uintLength w
      pokeByteOff p off (fromIntegral (k .|. 0x80) :: Word8)
      let go !j
            | j > k     = pure ()
            | otherwise = do
                pokeByteOff p (off + j) (fromIntegral (w `shiftR` (8 * (k - j))) :: Word8)
                go (j + 1)
      go 1
pokeLength p off LenIndefinite = pokeByteOff p off (0x80 :: Word8)

-- | The number of bytes in the shortest big-endian form of a word, at least
-- one.
uintLength :: Word64 -> Int
uintLength w = max 1 ((64 - countLeadingZeros w + 7) `div` 8)

errorShortLength, errorLongLength :: a
errorShortLength = error "putLength: short length is not between 0x0 and 0x80"
errorLongLength = error "putLength: long length is negative"
