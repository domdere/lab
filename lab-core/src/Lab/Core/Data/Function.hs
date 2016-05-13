-------------------------------------------------------------------
-- |
-- Module       : Lab.Core.Data.Function
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Lab.Core.Data.Function (
  -- * Functions
      invariantUnder
  ) where

import Test.QuickCheck ( Property, (===) )

import Preamble

invariantUnder :: (Show a, Show b, Eq b) => (a -> b) -> (a -> a) -> a -> Property
invariantUnder f t = (===) <$> f . t <*> f
