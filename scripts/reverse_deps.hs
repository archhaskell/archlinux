--
-- | This script output reverse dependencies for the specified package with
-- respect to a ABS-like repository rooted at the current directory
--

module Main where

import Distribution.ArchLinux.SrcRepo
import System.IO
import System.Directory
import System.Environment
import Control.Monad

main = do
  pkg <- getArgs
  dot <- getCurrentDirectory
  repo <- getRepoFromDir dot
  case repo of
    Nothing -> return ()
    Just r -> foldM (\a -> \s -> putStrLn s) () (getReverseDependencies pkg r)
