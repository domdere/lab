{-# LANGUAGE NoImplicitPrelude #-}
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
        monoidRightIdentity
    ,   monoidLeftIdentity
    ,   monoidAssociativity
    ) where

import Lab.Core.Hedgehog

import Data.Monoid ( (<>) )

import Preamble hiding ( (<>) )

monoidRightIdentity :: (Monoid a, Show a, Eq a) => a -> Property
monoidRightIdentity x = property $ x <> mempty === x

monoidLeftIdentity :: (Monoid a, Show a, Eq a) => a -> Property
monoidLeftIdentity x = property $ mempty <> x === x

monoidAssociativity :: (Monoid a, Show a, Eq a) => a -> a -> a -> Property
monoidAssociativity x y z = property $ ((x <> y) <> z) === (x <> (y <> z))
