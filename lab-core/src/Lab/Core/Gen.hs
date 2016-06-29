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
        bounded
    ,   boundedListOf
    ,   boundedListOf1
    ,   listOf1
    ,   maybeOf
    ) where

import Test.QuickCheck ( Gen, choose, frequency, listOf, vectorOf )

import System.Random ( Random )

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
