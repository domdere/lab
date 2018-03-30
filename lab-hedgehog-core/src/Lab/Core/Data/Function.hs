{-# LANGUAGE NoImplicitPrelude #-}
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

import Lab.Core.Hedgehog (
    MonadTest
  , (===)
  )

import Preamble

invariantUnder
  :: (MonadTest m, Show a, Show b, Eq b)
  => (a -> b)
  -> (a -> a)
  -> a
  -> m ()
invariantUnder f t = (===) <$> f . t <*> f
