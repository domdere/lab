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

import Test.QuickCheck ( NonNegative(..), Positive(..), Property, quickCheckAll )

import Preamble

prop_greaterThan :: Int -> Positive Int -> Property
prop_greaterThan x (Positive y) = (x + y) .>. x

prop_greaterThanOrEqual :: Int -> NonNegative Int -> Property
prop_greaterThanOrEqual x (NonNegative y) = (x + y) .>=. x

prop_lessThan :: Int -> Positive Int -> Property
prop_lessThan x (Positive y) = x .<. (x + y)

prop_lessThanOrEqual :: Int -> NonNegative Int -> Property
prop_lessThanOrEqual x (NonNegative y) = x .<=. (x + y)

prop_pass :: Property
prop_pass = pass

return []
tests :: IO Bool
tests = $quickCheckAll
