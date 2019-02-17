{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Lab.Core.IO
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Lab.Core.IO (ioProperty) where

import Lab.Core.QuickCheck

import Test.QuickCheck (Testable)
import Test.QuickCheck.Monadic (PropertyM, monadicIO, run, stop)

import Preamble

ioProperty :: forall a. (Testable a) => IO a -> Property
ioProperty mx = monadicIO $ do
  p <- run mx
  stop p :: PropertyM IO a
