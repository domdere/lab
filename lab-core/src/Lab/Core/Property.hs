-------------------------------------------------------------------
-- |
-- Module       : Lab.Core.Property
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Lab.Core.Property (
    -- * Operators
      (=/=)
    , (.>.)
    , (.>=.)
    , (.<.)
    , (.<=.)
    -- * Functions
    , failWith
    , isIn
    , pass
    ) where

import qualified Data.Text as T

import Test.QuickCheck (Property, counterexample)

import Preamble

infix 4 =/=
infix 4 .>.
infix 4 .>=.
infix 4 .<.
infix 4 .<=.

(=/=) :: (Show a, Eq a) => a -> a -> Property
x =/= y = counterexample (join [show x, " == ", show y]) (x /= y)

(.>.) :: (Ord a, Show a) => a -> a -> Property
x .>. y = counterexample (join [show x, " <= ", show y]) (x > y)

(.>=.) :: (Ord a, Show a) => a -> a -> Property
x .>=. y = counterexample (join [show x, " < ", show y]) (x >= y)

(.<.) :: (Ord a, Show a) => a -> a -> Property
x .<. y = counterexample (join [show x, " >= ", show y]) (x < y)

(.<=.) :: (Ord a, Show a) => a -> a -> Property
x .<=. y = counterexample (join [show x, " > ", show y]) (x <= y)

isIn :: (Show a, Show (t a), Eq a, Foldable t) => a -> t a -> Property
x `isIn` xs = counterexample (join [show x, " is not in ", show xs]) (x `elem` xs)

pass :: Property
pass = counterexample "" True

failWith :: T.Text -> Property
failWith t = counterexample (T.unpack t) False
