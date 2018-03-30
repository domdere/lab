{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
-------------------------------------------------------------------
-- |
-- Module       : Test.Lab.Core.Gen
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Lab.Core.Gen where

import Lab.Core.Gen
import Lab.Core.Property

import qualified Data.List as L
import qualified Data.Text as T

import Hedgehog (
    MonadGen
  , MonadTest
  , Gen
  , Property
  , (===)
  , checkParallel
  , discover
  , forAll
  , property
  )

import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

import Preamble

prop_distinctPairOf :: Property
prop_distinctPairOf = property $ do
  (x, y) <- forAll (distinctPairOf (G.int (R.constantFrom 0 (-1000) 1000)))
  x =/= y

prop_distinctListOfN :: Property
prop_distinctListOfN = testDistinctListOf distinctListOfN $ \n xs -> length xs === fromIntegral n

prop_distinctListOfN1 :: Property
prop_distinctListOfN1 = testDistinctListOf distinctListOfN1 $ \n xs -> length xs === fromIntegral (n + 1)

prop_boundedDistinctListOf :: Property
prop_boundedDistinctListOf = testDistinctListOf boundedDistinctListOf $ \n xs -> length xs .<=. fromIntegral n

prop_boundedDistinctListOf1 :: Property
prop_boundedDistinctListOf1 = testDistinctListOf boundedDistinctListOf1 $ \n xs -> length xs .<=. fromIntegral (n + 1)

testDistinctListOf
  :: (Foldable f, Show (f Int))
  => (Natural -> Gen Int -> Gen (f Int))
  -> (forall m a. (MonadTest m, Integral a) => a -> f Int -> m ())
  -> Property
testDistinctListOf f lengthProp = property $ do
  n <- forAll (G.integral (R.constantFrom 0 0 40))
  xs <- forAll (f n (G.int (R.constantFrom 0 (-1000) 1000)))
  let sorted' = L.sort $ toList xs
  group sorted' === fmap pure sorted'
  lengthProp n xs

tests :: IO Bool
tests = checkParallel $$discover
