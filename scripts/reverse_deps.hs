-- This script prints all reverse dependencies for the specified
-- packages with respect to an ABS-like repository located at the
-- path given as first command-line argument.

module Main where

import Distribution.ArchLinux.SrcRepo
import System.IO
import System.Directory
import System.Environment
import Control.Monad

main = do
  habs:pkgs <- getArgs
  repo <- getRepoFromDir habs
  print repo
  case repo of
    Nothing -> fail ("cannot load habs tree at " ++ show habs)
    Just r -> foldM (\a -> \s -> putStrLn s) () (getReverseDependencies pkgs r)
