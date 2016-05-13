-------------------------------------------------------------------
-- |
-- Module       : Lab.Core.IO
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Lab.Core.IO ( ioProperty ) where

import Lab.Core.QuickCheck

import Test.QuickCheck ( Testable )
import Test.QuickCheck.Monadic ( monadicIO, run, stop )

import Preamble

ioProperty :: (Testable a) => IO a -> Property
ioProperty = monadicIO . (=<<) stop . run
