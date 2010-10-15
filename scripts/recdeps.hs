--
-- | This test reads the current directory and dumps a topologically sorted package list
--

module Main where

import Distribution.ArchLinux.SrcRepo
import System.IO
import System.Directory
import System.Environment
import Control.Monad

main = do
  [pkg] <- getArgs
  dot <- getCurrentDirectory
  repo <- getRepoFromDir dot
  case repo of
    Nothing -> return ()
    Just r -> foldM (\a -> \s -> putStrLn s) () (getDependencies pkg r)
