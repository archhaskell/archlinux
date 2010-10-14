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
      contents <- foldM insertpkg empty subthings
      let result = SrcRepo { repo_path = path , repo_contents = contents }
      return (Just result)
    else return Nothing

insertpkg m dir = do
  pkg <- getPkgFromDir dir
  case pkg of
    Nothing -> return m
    Just p -> return $ insert (takeBaseName dir) p m
