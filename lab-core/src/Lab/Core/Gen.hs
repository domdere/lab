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
    arbitraryNatural
  , bounded
  , boundedDistinctListOf
  , boundedDistinctListOf1
  , boundedListOf
  , boundedListOf1
  , chooseNatural
  , distinctPairOf
  , distinctListOfN
  , distinctListOfN1
  , listOf1
  , maybeOf
  -- * Generators
  , textOf
  , textOf1
  , textOfN
  , boundedTextOf
  , boundedTextOf1
  -- * Values
  , numChars
  , alphaChars
  , alphaNumChars
  , punctuationSymbolChars
  , symbolChars
  ) where

import qualified Data.Text as T

import Test.QuickCheck (
    Arbitrary(..)
  , Gen
  , NonNegative(..)
  , choose
  , elements
  , frequency
  , listOf
  , suchThat
  , vectorOf
  )

import System.Random (Random)

import Preamble

bounded :: (Random a, Num a) => a -> (a -> Gen b) -> Gen b
bounded upper g = choose (0, abs upper) >>= g

boundedListOf :: Int -> Gen a -> Gen [a]
boundedListOf n g
    | n > 0     = choose (0, n) >>= flip vectorOf g
    | otherwise = pure []

-- |
-- returns a list with at least element from the generator,
-- if @n@ is less than 1, it is ignored and a singleton list is returned.
--
boundedListOf1 :: Int -> Gen a -> Gen (NonEmpty a)
boundedListOf1 n g = (:|) <$> g <*> boundedListOf (max 0 (n - 1)) g

-- |
-- generates a NonEmpty list using the given generator
listOf1 :: Gen a -> Gen (NonEmpty a)
listOf1 g = (:|) <$> g <*> listOf g

-- |
-- generates an Maybe using the given generator
--
maybeOf :: Gen a -> Gen (Maybe a)
maybeOf g = frequency [(1, pure Nothing), (3, pure <$> g)]

distinctPairOf :: (Eq a) => Gen a -> Gen (a, a)
distinctPairOf g = do
  x <- g
  y <- g `suchThat` (/= x)
  pure (x, y)

-- |
-- you dont want to use this with large n
--
distinctListOfN
  :: (Eq a)
  => Natural -- ^ @n@
  -> Gen a
  -> Gen [a]
distinctListOfN 0 _ = pure []
distinctListOfN n g = do
  x <- g
  xs <- distinctListOfN (n `monus` 1) (g `suchThat` (/= x))
  pure $ x:xs

-- |
-- Don't use this with large `n`
--
distinctListOfN1
  :: (Eq a)
  => Natural -- ^ @n@. Resultant list will be @n + 1@ elements long
  -> Gen a
  -> Gen (NonEmpty a)
distinctListOfN1 n g = do
  x <- g
  xs <- distinctListOfN n (g `suchThat` (/= x))
  pure $ x :| xs

arbitraryNatural :: Gen Natural
arbitraryNatural = (fromIntegral :: Int -> Natural) . getNonNegative <$> arbitrary

chooseNatural :: (Natural, Natural) -> Gen Natural
chooseNatural = fmap (fromIntegral :: Int -> Natural) . choose . bimap fromIntegral fromIntegral

boundedDistinctListOf :: (Eq a) => Natural -> Gen a -> Gen [a]
boundedDistinctListOf n g = chooseNatural (0, n) >>= \n' ->
  distinctListOfN n' g

boundedDistinctListOf1
  :: (Eq a)
  => Natural -- ^ The length of the generated non empty lists will be bounded by @n + 1@
  -> Gen a
  -> Gen (NonEmpty a)
boundedDistinctListOf1 n g = chooseNatural (0, n) >>= \n' ->
  distinctListOfN1 n' g

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

textOf :: [Char] -> Gen T.Text
textOf charset = fmap T.pack . listOf . elements $ charset

textOf1 :: [Char] -> Gen T.Text
textOf1 charset = fmap (T.pack . toList) . listOf1 . elements $ charset

textOfN :: Natural -> [Char] -> Gen T.Text
textOfN n charset = fmap T.pack . vectorOf (fromIntegral n) . elements $ charset

boundedTextOf :: Natural -> [Char] -> Gen T.Text
boundedTextOf n charset = fmap T.pack . boundedListOf (fromIntegral n) . elements $ charset

boundedTextOf1 :: Natural -> [Char] -> Gen T.Text
boundedTextOf1 n charset = fmap (T.pack . toList) . boundedListOf1 (fromIntegral n) . elements $ charset
