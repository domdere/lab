{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
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

import Hedgehog (
    Property
  , checkParallel
  , discover
  , forAll
  , property
  )

import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

import Preamble

import Prelude (mod)

prop_invariantUnder_modulo :: Property
prop_invariantUnder_modulo = property $ do
  n <- forAll . G.int $ R.constantFrom 1 1 100
  (forAll . G.int $ R.constantFrom 0 (-10000) 10000) >>=
    (`mod` n) `invariantUnder` (+ n)

tests :: IO Bool
tests = checkParallel $$discover
