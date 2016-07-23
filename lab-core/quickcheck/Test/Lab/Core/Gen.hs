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

import Test.QuickCheck (
    Arbitrary(..)
  , Gen
  , Property
  , (===)
  , conjoin
  , forAll
  , quickCheckAll
  )

import Preamble
import Prelude (Integral)

prop_distinctPairOf :: Property
prop_distinctPairOf = forAll (distinctPairOf (arbitrary :: Gen Int)) $ \(x, y) ->
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
  -> (forall a. (Integral a) => a -> f Int -> Property)
  -> Property
testDistinctListOf f lengthProp = forAll (chooseNatural (0, 40) >>= \n -> (,) n <$> f n arbitrary) $ \(n, xs) ->
  let
    sorted' :: [Int]
    sorted' = L.sort $ toList xs
  in conjoin [
      group sorted' === fmap pure sorted'
    , lengthProp n xs
    ]

return []
tests :: IO Bool
tests = $quickCheckAll
