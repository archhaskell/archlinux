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
import Distribution.Version

import Data.List as L
import Data.Map as M
import Data.Maybe
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
    deriving (Show)

--
-- | Reads a directory into a package
--
getPkgFromDir :: FilePath -> IO (Maybe PkgBuild)
getPkgFromDir p = do
  validdir <- Dir.doesFileExist (p </> "PKGBUILD")
  if validdir
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
      subthings' <- Dir.getDirectoryContents path
      let subthings = [ path </> x | x <- subthings', x /= ".", x /= ".." ]
      -- Read PkgBuilds
      contents <- foldM insertpkg M.empty subthings
      let result = SrcRepo { repo_path = path , repo_contents = contents }
      return (Just result)
    else return Nothing

insertpkg :: Map String PkgBuild -> FilePath -> IO (Map String PkgBuild)
insertpkg m dir = do
  pkg <- getPkgFromDir dir
  case pkg of
    Nothing -> fail $ "cannot read PKGBUILD from " ++ show dir
    Just p -> return $ M.insert (takeBaseName dir) p m

---------------------------------------------------------------------------
--
-- Only pure functions below

--
-- | Dumps a topologically sorted list of packages
-- starting with an optionally given key
--
dumpContentsTopo :: SrcRepo -> [String]
dumpContentsTopo repo
  | M.null m = []
  | otherwise = leafNames ++ (dumpContentsTopo $ repo {repo_contents = notLeaves})
  where -- find leaf packages
      m = repo_contents repo
      isLeaf pbuild = (trueDepends pbuild repo) == []
      leafList = L.filter (isLeaf . snd) (M.toList m)
      leafNames = L.map fst leafList
      notLeaves = M.filterWithKey (\n -> \pkg -> n `notElem` leafNames) m

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

trueDepends :: PkgBuild -> SrcRepo -> [String]
trueDepends p repo = L.filter (\p -> not $ isExternalDep p repo) (strDepends p)

------------------------------------------------------------

--
-- | Enumerate all build-time dependencies for a package
--
strDepends :: PkgBuild -> [String]
strDepends PkgBuild { arch_depends = ArchList deps
                    , arch_makedepends = ArchList makedeps }
                    = L.map pkgnameFromArchDep (deps ++ makedeps)

--
-- | Output the recursive dependencies of a package in topological order
--
getDependencies :: String -> SrcRepo -> [String]
getDependencies pkg repo = dumpContentsTopo $ getDependencyRepo pkg repo

--
-- | Extract the subrepository of recursive dependencies of a package
--
getDependencyRepo :: String -> SrcRepo -> SrcRepo
getDependencyRepo pkg repo = case M.lookup pkg $ repo_contents repo of
  Nothing -> repo { repo_contents = M.empty }
  Just p -> repo { repo_contents = M.insert pkg p (unions recDeps) }
              where trueDeps = trueDepends p repo
                    recDeps = L.map (repo_contents . (\d -> getDependencyRepo d repo)) trueDeps

--
-- | Output reverse dependencies of a list of packages in topological order
--
getReverseDependencies :: [String] -> SrcRepo -> [String]
getReverseDependencies pkg repo = dumpContentsTopo $ getReverseDependencyRepo pkg repo

--
-- | Extract reverse dependencies of a list of packages
--
getReverseDependencyRepo :: [String] -> SrcRepo -> SrcRepo
getReverseDependencyRepo pkgs repo = repo { repo_contents = revdeps }
  where revdeps = M.filterWithKey (isarevdep) (repo_contents repo)
        isarevdep k _ = or $ L.map (\p -> M.member p (repo_contents $ getDependencyRepo k repo)) pkgs

----------------------------------------------------------------
--
-- Version checking

--
-- | Find version inconsistencies in a repository
--
isConflicting :: SrcRepo -> Bool
isConflicting repo = and areConflicting
  where listOfPkgs = M.toList $ repo_contents repo
        areConflicting = L.map (\(k,pkg) -> pkg `isConflictingWith` repo) listOfPkgs

listVersionConflicts :: SrcRepo -> [String]
listVersionConflicts repo = L.map fst listConflicting
  where listOfPkgs = M.toList $ repo_contents repo
        listConflicting = L.filter (\(k,pkg) -> pkg `isConflictingWith` repo) listOfPkgs

--
-- | Check package dependencies against the repo
--
isConflictingWith :: PkgBuild -> SrcRepo -> Bool
PkgBuild { arch_depends = ArchList deps
         , arch_makedepends = ArchList makedeps
         } `isConflictingWith` repo = not (and satisfied)
  where satisfied = Data.Maybe.mapMaybe (\dep -> isSatisfiedBy dep repo) (deps ++ makedeps)

--
-- | check for existence of the right version is the repository
-- (return Nothing if package not found)
--
isSatisfiedBy :: ArchDep -> SrcRepo -> Maybe Bool
ArchDep (Dependency (PackageName depname) vrange) `isSatisfiedBy` repo = case deppkg of
    Nothing -> Nothing
    Just pkgbuild -> Just ((arch_pkgver pkgbuild) `withinRange` vrange)
  where
    deppkg = M.lookup depname (repo_contents repo)
