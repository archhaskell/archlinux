--
-- | This test reads the current directory and dumps a topologically sorted package list
--

import Distribution.ArchLinux.SrcRepo
import System.IO
import System.Directory
import Control.Monad

main = do
  dot <- getCurrentDirectory
  repo <- getRepoFromDir dot
  case repo of
    Nothing -> return ()
    Just r -> foldM (\a -> \s -> putStrLn s) () (dumpContentsTopo r)
