{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Lab.Core.Hedgehog.Gen
-- Copyright    : (C) 2017
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Lab.Core.Hedgehog.Gen (
  -- * Re-exports
    module X
  -- * Functions
  , element
  , choice
  , frequency
  , oneof
  ) where

import Hedgehog (MonadGen)

import Hedgehog.Gen as X hiding (
    element
  , choice
  , frequency
  )

import qualified Hedgehog.Gen as H

import Preamble

element :: (MonadGen m) => NonEmpty a -> m a
element = H.element . toList

choice :: (MonadGen m) => NonEmpty (m a) -> m a
choice = H.choice . toList

frequency :: (MonadGen m) => NonEmpty (Natural, m a) -> m a
frequency = H.frequency . toList . fmap (first fromIntegral)

oneof :: (MonadGen m) => NonEmpty (Natural, m a) -> m a
oneof = frequency
