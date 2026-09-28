{-# LANGUAGE LambdaCase #-}

{- |
Module      : Data.ASN1.Parse
License     : BSD-style
Copyright   : (c) 2010-2013 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

A monadic parser combinator for a stream of ASN.1 items.
-}

module Data.ASN1.Parse
  ( ParseASN1
    -- * Run 'ParseASN1'
  , runParseASN1State
  , runParseASN1
  , throwParseError
    -- * Combinators
  , onNextContainer
  , onNextContainerMaybe
  , getNextContainer
  , getNextContainerMaybe
  , getNext
  , getNextMaybe
  , hasNext
  , getObject
  , getMany
  ) where

import           Control.Applicative ( Alternative (..) )
import           Control.Arrow ( first )
import           Control.Monad ( liftM2 )
import           Data.ASN1.Types
                   ( ASN1 (..), ASN1ConstructionType (..), ASN1Object (..) )
import           Data.ASN1.Stream ( getConstructedEnd )

-- | The ASN.1 items a parser works on.
--
-- A parser run on the items of a construction ('onNextContainer') reads them
-- from the list they are in, up to the 'End' that closes the construction,
-- rather than from a copy of them. Which items are visible is exactly what
-- 'getConstructedEnd' would give.
data Stream
  = Items [ASN1]
    -- ^ All of a list.
  | Within !Int [ASN1]
    -- ^ A list up to the 'End' that closes a construction, given the number
    -- of 'Start' items read since the construction started that are still
    -- open.

-- | The items left in a stream, as a list.
items :: Stream -> [ASN1]
items (Items xs)    = xs
items (Within d xs) = fst (getConstructedEnd d xs)

-- | The next item in a stream, and the rest of the stream.
next :: Stream -> Maybe (ASN1, Stream)
next (Items [])       = Nothing
next (Items (x : xs)) = Just (x, Items xs)
next (Within d xs)    = case xs of
  []         -> Nothing
  (x : rest) -> case x of
    Start _        -> Just (x, Within (d + 1) rest)
    End _
      | d == 0     -> Nothing
      | otherwise  -> Just (x, Within (d - 1) rest)
    _              -> Just (x, Within d rest)
{-# INLINE next #-}

-- | The list under a stream.
underlying :: Stream -> [ASN1]
underlying (Items xs)    = xs
underlying (Within _ xs) = xs

-- | The stream, with the list under it replaced by what follows a whole
-- construction in it.
continueWith :: Stream -> [ASN1] -> Stream
continueWith (Items _)    xs = Items xs
continueWith (Within d _) xs = Within d xs

-- | Type representing a parser combinator for a stream of ASN.1 items.
newtype ParseASN1 a = P { runP :: Stream -> Either String (a, Stream) }

instance Functor ParseASN1 where
  fmap f m = P (fmap (first f) . runP m)

instance Applicative ParseASN1 where
  pure a = P $ \s -> Right (a, s)

  (<*>) mf ma = P $ \s ->
    case runP mf s of
      Left err      -> Left err
      Right (f, s2) ->
        case runP ma s2 of
          Left err      -> Left err
          Right (a, s3) -> Right (f a, s3)

instance Monad ParseASN1 where
  return = pure

  (>>=) m1 m2 = P $ \s ->
    case runP m1 s of
      Left err      -> Left err
      Right (a, s2) -> runP (m2 a) s2

instance Alternative ParseASN1 where
  empty = P $ \_ -> Left "empty Alternative"

  (<|>) m1 m2 = P $ \s ->
    case runP m1 s of
      Left _        -> runP m2 s
      Right (a, s2) -> Right (a, s2)

instance MonadFail ParseASN1 where
  fail = throwParseError

-- | Run the given parse monad over the given list of ASN.1 items. Returns the
-- result and a list of the ASN.1 items remaining in the stream (if successful).
runParseASN1State :: ParseASN1 a -> [ASN1] -> Either String (a, [ASN1])
runParseASN1State f list =
  case runP f (Items list) of
    Left err     -> Left err
    Right (a, s) -> Right (a, items s)

-- | Run the given parse monad over the given list of ASN.1 items and returns
-- the result (if successful).
--
-- If ASN.1 items remain in the stream after doing so, returns an error.
runParseASN1 :: ParseASN1 a -> [ASN1] -> Either String a
runParseASN1 f list =
  case runParseASN1State f list of
    Left err      -> Left err
    Right (result, []) -> Right result
    Right (_, rest) -> Left (remainingState rest)

remainingState :: [ASN1] -> String
remainingState rest = "runParseASN1: remaining state " ++ show rest

-- | Throw a parse error.
throwParseError ::
     String
     -- ^ Error message.
  -> ParseASN1 a
throwParseError err = P $ \_ -> Left err

-- | Run the parse monad over the elements of the next container of specified
-- type. Throws an error if there is no next container of the specified type.
onNextContainer :: ASN1ConstructionType -> ParseASN1 a -> ParseASN1 a
onNextContainer ty f = P $ \s ->
  case next s of
    Nothing -> Left "empty"
    Just (item, s2)
      | item == Start ty -> inContainer f s (underlying s2)
      | otherwise        -> Left "not an expected container"

-- | As for 'onNextContainer', except that it does not throw an error if there
-- is no next container of the specified type.
onNextContainerMaybe ::
     ASN1ConstructionType
  -> ParseASN1 a
  -> ParseASN1 (Maybe a)
onNextContainerMaybe ty f = P $ \s ->
  case next s of
    Just (item, s2)
      | item == Start ty -> first Just <$> inContainer f s (underlying s2)
    _ -> Right (Nothing, s)

-- | Run a parser on the items of a construction: those of a list up to the
-- 'End' that closes the construction, whose 'Start' has been read from the
-- given stream. All of them must be used. The stream then goes on after the
-- 'End'.
inContainer :: ParseASN1 a -> Stream -> [ASN1] -> Either String (a, Stream)
inContainer f s xs =
  case runP f (Within 0 xs) of
    Left err -> Left err
    Right (a, Within d ys) -> case ys of
      [] -> Right (a, continueWith s [])
      (y : rest) -> case y of
        End _ | d == 0 -> Right (a, continueWith s rest)
        _              -> Left (remainingState (fst (getConstructedEnd d ys)))
    -- The parser has turned the items into a list of its own ('getObject').
    Right (a, Items []) -> Right (a, continueWith s (snd (getConstructedEnd 0 xs)))
    Right (_, Items rest) -> Left (remainingState rest)

-- | Get the next container of the specified type and return a list of all its
-- ASN.1 elements. Throws a parse error if there is no next container of the
-- specified type.
getNextContainer :: ASN1ConstructionType -> ParseASN1 [ASN1]
getNextContainer ty = P $ \s ->
  case next s of
    Nothing -> Left "empty"
    Just (item, s2)
      | item == Start ty ->
          let (list, rest') = getConstructedEnd 0 (underlying s2)
          in  Right (list, continueWith s rest')
      | otherwise -> Left "not an expected container"

-- | As for 'getNextContainer', except that it does not throw an error if there
-- is no next container of the specified type.
getNextContainerMaybe :: ASN1ConstructionType -> ParseASN1 (Maybe [ASN1])
getNextContainerMaybe ty = P $ \s ->
  case next s of
    Just (item, s2)
      | item == Start ty ->
          let (list, rest') = getConstructedEnd 0 (underlying s2)
          in  Right (Just list, continueWith s rest')
    _ -> Right (Nothing, s)

-- | Get the next ASN.1 item in a stream of ASN.1 items.
getNext :: ParseASN1 ASN1
getNext = P $ \s ->
  case next s of
    Nothing        -> Left "empty"
    Just (item, s2) -> Right (item, s2)

-- | Applies the given function to the next ASN.1 item in a stream of ASN.1
-- items, if there is one.
getNextMaybe :: (ASN1 -> Maybe a) -> ParseASN1 (Maybe a)
getNextMaybe f = P $ \s ->
  case next s of
    Nothing        -> Right (Nothing, s)
    Just (item, s2) ->
      let result = f item
      in  case result of
            Nothing -> Right (result, s)
            Just _  -> Right (result, s2)

-- | Are there any more ASN.1 items in the stream?
hasNext :: ParseASN1 Bool
hasNext = P $ \s -> Right (maybe False (const True) (next s), s)

-- | Get the object from the next ASN.1 item in a stream of ASN.1 items.
-- Throws a parse error if the object cannot be obtained from the item.
getObject :: ASN1Object a => ParseASN1 a
getObject = P $ \s ->
  case fromASN1 (items s) of
    Left err             -> Left err
    Right (object, rest) -> Right (object, Items rest)

-- | Get many items from the stream until there are none left.
getMany :: ParseASN1 a -> ParseASN1 [a]
getMany getOne =
  hasNext >>= \case
    True -> liftM2 (:) getOne (getMany getOne)
    False -> pure []
