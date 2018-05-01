module Lab.Core.Main ( labMain, labCliMain ) where

import Data.Foldable ( and )
import Preamble
import System.Directory (getDirectoryContents)
import System.Exit ( exitFailure )
import System.IO ( BufferMode(..), hSetBuffering, stdout, stderr)
import System.Process (callProcess)

labMain :: [IO Bool] -> IO ()
labMain tests = sanity >> sequence tests >>= \rs -> unless (and rs) exitFailure

labCliMain :: [String] -> IO ()
labCliMain arguments =
  let ignore p = ".." == p || "." == p || "core" == p
      exec t = callProcess ("test/cli/" ++ t ++ "/run") arguments
   in sanity >> filter (not . ignore) <$> getDirectoryContents "test/cli/" >>= mapM_ exec

sanity :: IO ()
sanity =
  hSetBuffering stdout LineBuffering >> hSetBuffering stderr LineBuffering
