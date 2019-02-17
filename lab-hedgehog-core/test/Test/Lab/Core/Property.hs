{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Lab.Core.Property
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Lab.Core.Property where

import Lab.Core.Property

import Hedgehog (
    MonadGen
  , Property
  , forAll
  , checkParallel
  , discover
  , property
  )

import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

import Preamble

prop_greaterThan :: Property
prop_greaterThan = property $ do
  x <- forAll $ G.int (R.constantFrom 0 (-100) 100)
  y <- forAll $ G.int (R.constantFrom 1 1 100)
  (x + y) .>. x

prop_greaterThanOrEqual :: Property
prop_greaterThanOrEqual = property $ do
  x <- forAll $ G.int (R.constantFrom 0 (-100) 100)
  y <- forAll $ G.int (R.constantFrom 0 0 100)
  (x + y) .>=. x

prop_lessThan :: Property
prop_lessThan = property $ do
  x <- forAll $ G.int (R.constantFrom 0 (-100) 100)
  y <- forAll $ G.int (R.constantFrom 1 1 100)
  x .<. (x + y)

prop_lessThanOrEqual :: Property
prop_lessThanOrEqual = property $ do
  x <- forAll $ G.int (R.constantFrom 0 (-100) 100)
  y <- forAll $ G.int (R.constantFrom 0 0 100)
  x .<=. (x + y)

prop_isIn :: Property
prop_isIn =
  let
    g :: (MonadGen m) => m (Int, [Int])
    g = do
      x <- G.int $ R.constantFrom 0 (-1000) 1000
      xs <- G.list (R.constantFrom 0 0 40) (G.int (R.constantFrom 0 (-1000) 1000))
      shuffled <- G.shuffle $ x:xs
      pure (x, shuffled)
  in property $ do
    (x, xs) <- forAll g
    x `isIn` xs

tests :: IO Bool
tests = checkParallel $$discover
