module Main where

import qualified Test.Lab.Core.Data.Function
import qualified Test.Lab.Core.Gen
import qualified Test.Lab.Core.Property

import Lab.Core.Main

import System.IO (IO)

main :: IO ()
main = labMain
  [ Test.Lab.Core.Data.Function.tests
  , Test.Lab.Core.Gen.tests
  , Test.Lab.Core.Property.tests
  ]
