-------------------------------------------------------------------
-- |
-- Module       : Lab.Core.Control.RoundTrip
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- Generalised Round Trip Property
--
-------------------------------------------------------------------
module Lab.Core.Control.RoundTrip (
    -- * Functions
        roundTripProp
    ) where

import Test.QuickCheck ( Property, (===) )

import Preamble

roundTripProp :: (Applicative f, Show (f a), Eq (f a)) => (a -> b) -> (b -> f a) -> a -> Property
roundTripProp to from = (===) <$> from . to <*> pure
