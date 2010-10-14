-- |
-- Module    : Distribution.ArchLinux.SrcRepo
-- Copyright : (c) Rémy Oudompheng, 2010
-- License   : BSD3
--
-- This module makes a database out a directory ${repo}
-- containing subdirs ${repo}/${package} corresponding to packages.
-- It can output rebuild lists in reverse dependency order.

module Distribution.ArchLinux.SrcRepo where

import Distribution.ArchLinux.PkgBuild as PkgBuild

import Distribution.Package
import Distribution.Text
import Distribution.Version
import Text.PrettyPrint

import Data.List as L
import Data.Map as M
import System.FilePath
import System.Directory as Dir
import Control.Monad

--
-- | Data type for source repositories
--
data SrcRepo = SrcRepo
    { repo_path :: FilePath
        -- ^
        -- The path to the repository
    , repo_contents :: M.Map String PkgBuild
    }

--
-- | Reads a directory into a package
--
getPkgFromDir :: FilePath -> IO (Maybe PkgBuild)
getPkgFromDir p = do
  valid <- Dir.doesFileExist (p </> "PKGBUILD")
  if valid
    then do
      pkg <- readFile (p </> "PKGBUILD")
      case decodePackage pkg of
        Left _ -> return Nothing
        Right annot_pkg -> return $ Just (pkgBody annot_pkg)
    else return Nothing

--
-- | Reads a specified path into a SrcRepo structure
--
getRepoFromDir :: FilePath -> IO (Maybe SrcRepo)
getRepoFromDir path = do
  valid <- Dir.doesDirectoryExist path
  if valid
    then do
      subthings <- Dir.getDirectoryContents path
      -- Read PkgBuilds
      contents <- foldM insertpkg M.empty subthings
      let result = SrcRepo { repo_path = path , repo_contents = contents }
      return (Just result)
    else return Nothing

insertpkg m dir = do
  pkg <- getPkgFromDir dir
  case pkg of
    Nothing -> return m
    Just p -> return $ M.insert (takeBaseName dir) p m

--
-- | Dumps a topologically sorted list of packages
-- starting with an optionally given key
-- TODO : version checking
--
dumpContentsTopo :: SrcRepo -> [String]
dumpContentsTopo repo =
  -- find leaf packages
  let m = repo_contents repo
      isLeaf pbuild = (trueDepends pbuild repo) == []
      leafList = L.filter (isLeaf . snd) (M.toList m)
      leafNames = L.map fst leafList
      notLeaves = M.filterWithKey (\n -> \pkg -> n `notElem` leafNames) m
  in leafNames ++ (dumpContentsTopo $ SrcRepo {repo_contents = notLeaves})

--- We temporarily duplicate here the list of pseudo-dependencies
archProvidedPkgs :: [String]
archProvidedPkgs =
 [ "ghc"
 , "haskell-array", "haskell-bytestring", "haskell-cabal", "haskell-containers", "haskell-directory"
 , "haskell-extensible-exceptions", "haskell-filepath", "haskell-haskell98", "haskell-hpc", "haskell-old-locale"
 , "haskell-old-time", "haskell-pretty", "haskell-process", "haskell-random", "haskell-syb", "haskell-template-haskell", "haskell-time"
 , "haskell-unix" ]

--
-- | Helper function
--
isExternalDep :: String -> SrcRepo -> Bool
isExternalDep name (SrcRepo {repo_contents = m}) =
  (name `notMember` m) || (name `elem` archProvidedPkgs)

strDepends :: PkgBuild -> [String]
strDepends PkgBuild { arch_depends = ArchList deps } = L.map pkgnameFromArchDep deps

trueDepends :: PkgBuild -> SrcRepo -> [String]
trueDepends p repo = L.filter (\p -> not $ isExternalDep p repo) (strDepends p)