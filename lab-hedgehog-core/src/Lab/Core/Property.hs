{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
-------------------------------------------------------------------
-- |
-- Module       : Lab.Core.Property
-- Copyright    : (C) 2018
-- License      : BSD-style (see the file /LICENSE.md)
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
    , isIn
    ) where

import Data.List (unlines)

import Hedgehog (
    MonadTest
  , (/==)
  , eval
  , success
  )

import Hedgehog.Internal.Property (failWith)
import Hedgehog.Internal.Source (withFrozenCallStack)
import Hedgehog.Internal.Show (showPretty)

import Preamble

infix 4 =/=
infix 4 .>.
infix 4 .>=.
infix 4 .<.
infix 4 .<=.

(=/=) :: (MonadTest m, Show a, Eq a) => a -> a -> m ()
x =/= y = x /== y

(.>.) :: (MonadTest m, Ord a, Show a) => a -> a -> m ()
x .>. y = withFrozenCallStack (eval (x > y)) >>= \case
  True -> success
  False -> withFrozenCallStack $
    failWith Nothing $ unlines [
        "━━━ Less than or equal to ━━━"
      , showPretty x <> " <= " <> showPretty y
      ]

(.>=.) :: (MonadTest m, Ord a, Show a) => a -> a -> m ()
x .>=. y = withFrozenCallStack (eval (x >= y)) >>= \case
  True -> success
  False -> withFrozenCallStack $
    failWith Nothing $ unlines [
        "━━━ Less than  ━━━"
      , showPretty x <> " < " <> showPretty y
      ]

(.<.) :: (MonadTest m, Ord a, Show a) => a -> a -> m ()
x .<. y = withFrozenCallStack (eval (x < y)) >>= \case
  True -> success
  False -> withFrozenCallStack $
    failWith Nothing $ unlines [
        "━━━ Greater than or equal to  ━━━"
      , showPretty x <> " >= " <> showPretty y
      ]

(.<=.) :: (MonadTest m, Ord a, Show a) => a -> a -> m ()
x .<=. y = withFrozenCallStack (eval (x <= y)) >>= \case
  True -> success
  False -> withFrozenCallStack $
    failWith Nothing $ unlines [
        "━━━ Greater than ━━━"
      , showPretty x <> " > " <> showPretty y
      ]

isIn :: (MonadTest m, Show a, Show (t a), Eq a, Foldable t) => a -> t a -> m ()
x `isIn` xs = withFrozenCallStack (eval (x `elem` xs)) >>= \case
  True -> success
  False -> withFrozenCallStack $
    failWith Nothing $ unlines [
        "━━━ Element not found in collection ━━━"
      , showPretty x <> " was not found in " <> showPretty xs
      ]
