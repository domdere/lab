-------------------------------------------------------------------
-- |
-- Module       : Lab.Core.QuickCheck
-- Copyright    : (C) 2015
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-- A re export of commonly used QuickCheck types and Functions,
-- also saves the trouble of having to specify a QuickCheck version
-- in the test-suites hopefully... (before cabal crumbles under
-- the pressure of the deep dep tree that is)
--
-------------------------------------------------------------------
module Lab.Core.QuickCheck (
    -- * Re-exports
        module X
    ) where

import Test.QuickCheck as X (
        Arbitrary(..)
    ,   Args(..)
    ,   Gen
    ,   NonNegative(..)
    ,   Positive(..)
    ,   Property
    ,   (.&&.)
    ,   (.||.)
    ,   (===)
    ,   (==>)
    ,   choose
    ,   conjoin
    ,   counterexample
    ,   disjoin
    ,   elements
    ,   forAll
    ,   frequency
    ,   oneof
    ,   listOf
    ,   stdArgs
    ,   suchThat
    ,   vectorOf
    ,   quickCheck
    ,   quickCheckWithResult
    )
