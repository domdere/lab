module Lab.Core.Main ( labMain ) where

import Data.Foldable ( and )
import System.Exit ( exitFailure )
import System.IO ( BufferMode(..), hSetBuffering, stdout )

import Preamble

labMain :: [IO Bool] -> IO ()
labMain tests = hSetBuffering stdout LineBuffering >> sequence tests >>= \rs -> unless (and rs) exitFailure
