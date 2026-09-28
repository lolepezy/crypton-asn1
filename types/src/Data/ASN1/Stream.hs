{-# LANGUAGE BangPatterns #-}

{- |
Module      : Data.ASN1.Stream
License     : BSD-style
Copyright   : (c) 2010-2013 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown
-}

module Data.ASN1.Stream
  ( ASN1Repr
    -- * Utilities
  , getConstructedEnd
  , getConstructedEndRepr
  ) where

import           Data.ASN1.Types ( ASN1 (..) )
import           Data.ASN1.Types.Lowlevel ( ASN1Event )

-- | Type synonym representing pairs of a ASN.1 value and a list of ASN.1
-- events.
--
-- This association is sometimes needed in order to know the exact byte sequence
-- leading to an ASN.1 value. For example, in the case of a cryptographic
-- signature.
type ASN1Repr = (ASN1, [ASN1Event])

-- | For the given list of ASN.1 values, assumed to follow a 'Start' value:
--
-- If the list is empty, return a pair of empty lists.
--
-- Otherwise, return a list of values up to (but excluding) the corresponding
-- 'End' value (if any), and a list of the remaining ASN.1 values.
--
-- Both lists are lazy: the values in the first are looked at as it is
-- consumed, and those up to the corresponding 'End' when the second is.
getConstructedEnd ::
     Int
     -- ^ The number of additional 'Start' values encountered, @0@ initially.
  -> [ASN1]
  -> ([ASN1], [ASN1])
getConstructedEnd i xs = case xs of
  []      -> ([], [])
  (x : _) -> x `seq` (constructedPrefix i xs, constructedRest i xs)

-- | The values up to (but excluding) the corresponding 'End'.
constructedPrefix :: Int -> [ASN1] -> [ASN1]
constructedPrefix !i xs = case xs of
  []         -> []
  (x : rest) -> case x of
    Start _
      -> x : constructedPrefix (i + 1) rest
    End _
      | i == 0    -> []
      | otherwise -> x : constructedPrefix (i - 1) rest
    _ -> x : constructedPrefix i rest

-- | The values after the corresponding 'End'.
constructedRest :: Int -> [ASN1] -> [ASN1]
constructedRest !i xs = case xs of
  []         -> []
  (x : rest) -> case x of
    Start _
      -> constructedRest (i + 1) rest
    End _
      | i == 0    -> rest
      | otherwise -> constructedRest (i - 1) rest
    _ -> constructedRest i rest

-- | For the given list of 'ASN1Repr' pairs:
--
-- If the list is empty, return a pair of empty lists.
--
-- If the first item represents a 'Start' value, return a list of pairs up to
-- the corresponding 'End' value (if any) (including the 'Start' and any 'End')
-- and a list of the remaining 'ASN1Repr' pairs.
--
-- Otherwise, return a list of that first item and a list of the
-- remaining pairs.
getConstructedEndRepr :: [ASN1Repr] -> ([ASN1Repr], [ASN1Repr])
getConstructedEndRepr xs = case xs of
  -- The given list is empty.
  [] -> ([], [])
  (x@(a, _) : rest) -> case a of
    -- The first item represents a 'Start'.
    Start _ -> (x : reprPrefix 1 rest, reprRest 1 rest)
    -- The first item does not represent a 'Start'.
    _       -> ([x], rest)

-- | Given the number of 'Start' values encountered, the pairs up to (and
-- including) the corresponding 'End'.
reprPrefix :: Int -> [ASN1Repr] -> [ASN1Repr]
reprPrefix !i xs = case xs of
  [] -> []
  (x : rest)
    -- There is no, or no longer a, corresponding 'Start'.
    | i == 0    -> []
    | otherwise -> case x of
        (Start _, _) -> x : reprPrefix (i + 1) rest
        (End _, _)   -> x : reprPrefix (i - 1) rest
        _            -> x : reprPrefix i rest

-- | Given the number of 'Start' values encountered, the pairs after the
-- corresponding 'End'.
reprRest :: Int -> [ASN1Repr] -> [ASN1Repr]
reprRest !i xs = case xs of
  [] -> []
  (x : rest)
    | i == 0    -> xs
    | otherwise -> case x of
        (Start _, _) -> reprRest (i + 1) rest
        (End _, _)   -> reprRest (i - 1) rest
        _            -> reprRest i rest
