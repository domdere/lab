-------------------------------------------------------------------
-- |
-- Module       : Test.Lab.Core.Control.RoundTrip
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- Generalised Round Trip Property
--
-------------------------------------------------------------------
module Test.Lab.Core.Control.RoundTrip ( tests ) where

import Lab.Core.Control.RoundTrip

import Test.QuickCheck ( Arbitrary, Property, quickCheckAll )

import Preamble

prop_roundTrip_pure :: (Arbitrary a, Show a, Eq a) => a -> Property
prop_roundTrip_pure = roundTripProp id Just

return []
tests :: IO Bool
tests = $quickCheckAll
