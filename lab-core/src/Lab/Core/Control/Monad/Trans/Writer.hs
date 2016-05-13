-------------------------------------------------------------------
-- |
-- Module       : Lab.Core.Control.Monad.Trans.Writer
-- Copyright    : (C) 2016
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- WriterT is an anti-pattern for any important code, but it
-- comes in handy for test code.
-------------------------------------------------------------------
module Lab.Core.Control.Monad.Trans.Writer (
        module X
    ) where

import Control.Monad.Trans.Writer as X (
        Writer
    ,   WriterT(..)
    ,   listen
    ,   pass
    ,   tell
    ,   writer
    )
