-------------------------------------------------------------------
-- |
-- Module       : Lab.Core.Data.Monoid
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Lab.Core.Data.Monoid (
    -- * Functions
        monoidLaws
    ,   monoidRightIdentity
    ,   monoidLeftIdentity
    ,   monoidAssociativity
    ) where

import Lab.Core.QuickCheck

import Data.Monoid ( (<>) )

import Preamble hiding ( (<>) )

monoidRightIdentity :: (Monoid a, Show a, Eq a) => a -> Property
monoidRightIdentity x = x <> mempty === x

monoidLeftIdentity :: (Monoid a, Show a, Eq a) => a -> Property
monoidLeftIdentity x = mempty <> x === x

monoidAssociativity :: (Monoid a, Show a, Eq a) => a -> a -> a -> Property
monoidAssociativity x y z = ((x <> y) <> z) === (x <> (y <> z))

monoidLaws :: (Monoid a, Show a, Eq a) => Gen a -> Property
monoidLaws g = forAll g $ \x -> forAll g $ \y -> forAll g $ \z -> monoidLaws' x y z

monoidLaws' :: (Monoid a, Show a, Eq a) => a -> a -> a -> Property
monoidLaws' x y z = conjoin [
        monoidRightIdentity x
    ,   monoidRightIdentity y
    ,   monoidRightIdentity z
    ,   monoidLeftIdentity x
    ,   monoidLeftIdentity y
    ,   monoidLeftIdentity z
    ,   monoidAssociativity x y z
    ]
