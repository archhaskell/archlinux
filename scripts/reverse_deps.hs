-- This script prints all reverse dependencies for the specified
-- packages with respect to an ABS-like repository located at the
-- path given as first command-line argument.

module Main ( main ) where

import System.Environment ( getArgs )
import Distribution.ArchLinux.SrcRepo ( getRepoFromDir, getReverseDependencies )

main :: IO ()
main = do
  habs:pkgs <- getArgs
  repo <- getRepoFromDir habs
  case repo of
    Nothing -> fail ("cannot load habs tree at " ++ show habs)
    Just r -> mapM_ putStrLn (filter (`notElem`pkgs) (getReverseDependencies pkgs r))
