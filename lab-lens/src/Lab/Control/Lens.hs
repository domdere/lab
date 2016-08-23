{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Lab.Control.Lens
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Lab.Control.Lens (
  --  Traversal Laws
    traversalLaws
  -- Lens Laws
  , lensLaws
  -- Prism Laws
  , prismLaws
  -- Setter Laws
  , setterLaws
  ) where

import Lab.Core.QuickCheck (Property, (===), conjoin)

import Control.Lens (Lens', Prism', Setter', Traversal', (^?), (^.), (.~), mapped, re)

import Preamble

-- Traversal Laws

-- |
-- The `fmap (t f) . t g ≡ getCompose . t (Compose . fmap f . g)` law isn't checked
-- but i think its unnecessary, coupled with parametricity, the pure law should suffice.
--
traversalLaws :: (Show s, Eq s) => Traversal' s a -> s -> Property
traversalLaws t x = t Just x === Just x

-- Prism Laws

prismSymmetry :: (Show a, Eq a) => Prism' s a -> a -> Property
prismSymmetry l y = (y ^. re l) ^? l === pure y

prismConverseSymmetry :: (Show s, Eq s) => Prism' s a -> s -> Property
prismConverseSymmetry l x = fmap (^. re l) (x ^? l) === (mapped .~ x) (x ^? l)

-- |
-- Prisms must satisfy the Symmetry and Converse Symmetry laws as well as the Traversal Laws.
--
prismLaws :: (Show a, Eq a, Show s, Eq s) => Prism' s a -> s -> a -> Property
prismLaws p x y = conjoin
  [ prismSymmetry p y
  , prismConverseSymmetry p x
  , traversalLaws p x
  ]

-- Lens Laws

-- |
-- You get back out what you put in:
--
lensSetThenView :: (Show s, Show a, Eq a) => Lens' s a -> s -> a -> Property
lensSetThenView l x y = (l .~ y) x ^. l === y

lensViewThenSet :: (Show s, Eq s) => Lens' s a -> s -> Property
lensViewThenSet l x = (l .~ (x ^. l)) x === x

setIdempotent :: forall s a. (Show s, Show a, Eq s) => Lens' s a -> s -> a -> Property
setIdempotent l x y =
  let
    set :: s -> s
    set = l .~ y
  in  set (set x) === set x

lensLaws :: forall s a. (Show s, Show a, Eq s, Eq a) => Lens' s a -> s -> a -> Property
lensLaws l x y = conjoin
  [ lensSetThenView l x y
  , lensViewThenSet l x
  , setIdempotent l x y
  ]

-- setterLaws
setterLaws :: forall s a. (Show s, Show a, Eq s, Eq a) => Setter' s a -> s -> a -> a -> Property
setterLaws l x y y' = ((l .~ y) . (l .~ y') $ x) === (l .~  y) x

