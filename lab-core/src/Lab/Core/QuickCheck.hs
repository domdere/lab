{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Lab.Core.QuickCheck
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- A re export of commonly used QuickCheck types and Functions,
-- also saves the trouble of having to specify a QuickCheck version
-- in the test-suites hopefully... (before cabal crumbles under
-- the pressure of the deep dep tree that is)
--
-------------------------------------------------------------------
module Lab.Core.QuickCheck (
  -- * Re-exports
    module X
  -- * Modifications
  , elements
  , frequency
  , oneof
  ) where

import Test.QuickCheck as X (
    Arbitrary(..)
  , Args(..)
  , Gen
  , NonNegative(..)
  , Positive(..)
  , Property
  , (.&&.)
  , (.||.)
  , (===)
  , (==>)
  , choose
  , conjoin
  , counterexample
  , disjoin
  , forAll
  , listOf
  , stdArgs
  , suchThat
  , vectorOf
  , quickCheck
  , quickCheckWithResult
  )

import qualified Test.QuickCheck as Q

import Preamble

elements :: NonEmpty a -> Gen a
elements = Q.elements . toList

frequency :: NonEmpty (Natural, Gen a) -> Gen a
frequency = Q.frequency . toList . fmap (first fromIntegral)

oneof :: NonEmpty (Gen a) -> Gen a
oneof = Q.oneof . toList
