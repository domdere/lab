-------------------------------------------------------------------
-- |
-- Module       : Test.Lab.Core.Data.Function
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Test.Lab.Core.Data.Function ( tests ) where

import Lab.Core.Data.Function

import Test.QuickCheck ( Positive(..), Property, quickCheckAll )

import Preamble

import Prelude ( mod )

prop_invariantUnder_modulo :: Positive Int -> Int -> Property
prop_invariantUnder_modulo (Positive n) = (`mod` n) `invariantUnder` (+ n)

return []
tests :: IO Bool
tests = $quickCheckAll
