-- |
-- Module    : Distribution.ArchLinux.HackageTranslation
-- Copyright : (c) Rémy Oudompheng 2010
-- License   : BSD3
--
-- Maintainer: Arch Haskell Team <arch-haskell@haskell.org>
--

module Distribution.ArchLinux.HackageTranslation
  ( getVersionConflicts
  , getLatestVersions
  )
  where

import Distribution.ArchLinux.CabalTranslation
import Distribution.ArchLinux.SystemProvides
-- Cabal modules
import Distribution.Package
import Distribution.Version
import Distribution.PackageDescription
-- Standard types
import qualified Data.Map as M
import Data.Maybe

--
-- | Check for inconsistencies in version requirements
-- returns a list of pairs (package, conflicting dep).
--
getVersionConflicts :: [GenericPackageDescription] -> SystemProvides -> [(PackageDescription, Dependency)]
getVersionConflicts packages sysProvides = concat $ map conflicts cabals
  where cabals = mapMaybe (\p -> preprocessCabal p sysProvides) packages
        versions = M.fromList $ map (\p -> (pkgName $ packageId p, pkgVersion $ packageId p)) cabals
        issatisfied (Dependency pkg range) = case M.lookup pkg versions of
                                                 Nothing -> True
                                                 Just v -> v `withinRange` range
        conflicts p = map (\d -> (p,d)) $ filter (not . issatisfied) (buildDepends p)

--
-- | Returns the latest versions
--
getLatestVersions :: [GenericPackageDescription] -> M.Map PackageName Version
getLatestVersions packages = M.fromListWith max versions
  where versions = map (\p -> (pkgName $ packageId p, pkgVersion $ packageId p)) packages
