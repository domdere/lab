{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Lab.Core.Gen
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Lab.Core.Gen (
  -- * Generator Combinators
    boundedDistinctListOf
  , boundedDistinctListOf1
  , distinctPairOf
  , distinctListOfN
  , distinctListOfN1
  -- * Values
  , hexChars
  , hexCharsUpperCase
  , numChars
  , alphaChars
  , alphaNumChars
  , punctuationSymbolChars
  , symbolChars
  ) where

import Lab.Core.Hedgehog
import qualified Lab.Core.Hedgehog.Gen as G
import qualified Lab.Core.Hedgehog.Range as R

import Preamble

distinctPairOf :: (MonadGen m, Eq a) => m a -> m (a, a)
distinctPairOf g = do
  x <- g
  y <- G.filter (/= x) g
  pure (x, y)

-- |
-- you dont want to use this with large n
--
distinctListOfN
  :: (MonadGen m, Eq a)
  => Natural -- ^ @n@
  -> m a
  -> m [a]
distinctListOfN 0 _ = pure []
distinctListOfN n g = do
  x <- g
  xs <- distinctListOfN (n `monus` 1) (G.filter (/= x) g)
  pure $ x:xs

-- |
-- Don't use this with large `n`
--
distinctListOfN1
  :: (MonadGen m, Eq a)
  => Natural -- ^ @n@. Resultant list will be @n + 1@ elements long
  -> m a
  -> m (NonEmpty a)
distinctListOfN1 n g = do
  x <- g
  xs <- distinctListOfN n (G.filter (/= x) g)
  pure $ x :| xs

boundedDistinctListOf :: (MonadGen m, Eq a) => Natural -> m a -> m [a]
boundedDistinctListOf n g = G.integral (R.constantFrom 0 0 n) >>= \n' ->
  distinctListOfN n' g

boundedDistinctListOf1
  :: (MonadGen m, Eq a)
  => Natural -- ^ The length of the generated non empty lists will be bounded by @n + 1@
  -> m a
  -> m (NonEmpty a)
boundedDistinctListOf1 n g = G.integral (R.constantFrom 0 0 n) >>= \n' ->
  distinctListOfN1 n' g

hexChars :: [Char]
hexChars = "0123456789abcdef"

hexCharsUpperCase :: [Char]
hexCharsUpperCase = "0123456789ABCDEF"

numChars :: [Char]
numChars = "0123456789"

alphaChars :: [Char]
alphaChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

alphaNumChars :: [Char]
alphaNumChars = numChars <> alphaChars

punctuationSymbolChars :: [Char]
punctuationSymbolChars = "?!.,:;'\""

symbolChars :: [Char]
symbolChars = "[]{}|\\/+=()@#$%^&*`~_-"
