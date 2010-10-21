-- |
-- Module    : Distribution.ArchLinux.HackageTranslation
-- Copyright : (c) Rémy Oudompheng 2010
-- License   : BSD3
--
-- Maintainer: Arch Haskell Team <arch-haskell@haskell.org>
--

module Distribution.ArchLinux.HackageTranslation where
-- Cabal modules
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
-- Standard types
import Distribution.Text
--import Data.List
import qualified Data.Set as Set
import Data.Maybe
-- Read tarballs
import qualified Codec.Archive.Tar as Tar
import qualified Data.ByteString.Lazy.Char8 as Bytes
-- Debugging
import Debug.Trace

--
-- | Reads a tarball and converts it to a list of PackageDescription's
--
getCabalsFromTarball :: Bytes.ByteString -> [GenericPackageDescription]
getCabalsFromTarball tarball = Tar.foldEntries insertThis [] (const []) files
  where files = Tar.read tarball
        insertThis file list = case getCabalFromEntry file of
          Nothing -> list
          Just pkg -> pkg:list
        
getCabalFromEntry :: Tar.Entry -> Maybe GenericPackageDescription
getCabalFromEntry file = case Tar.entryContent file of
  Tar.NormalFile contents _ -> parse2maybe $ parsePackageDescription $ Bytes.unpack contents
  _ -> Nothing

parse2maybe a = case a of
      ParseOk _ pkg -> Just pkg
      _ -> Nothing

--
-- | Reads a tarball and get cabal files according to a list
--
getSpecifiedCabalsFromTarball :: Bytes.ByteString -> [String] -> [GenericPackageDescription]
getSpecifiedCabalsFromTarball tarball list =
  getSpecifiedCabals (mapMaybe parsePackageIdentifier list) (getCabalsFromTarball tarball)

--
-- | Parses a list of lines of the form "package-name 1.2.3.4"
--
parsePackageIdentifier :: String -> Maybe PackageIdentifier
parsePackageIdentifier s = case words s of
  name:version:_ -> case ver of
        Nothing -> void
        Just v -> Just $ PackageIdentifier { pkgName = PackageName name ,
                                       pkgVersion = v }
      where ver = simpleParse version
  _ -> void
 where
  void = Debug.Trace.trace("Malformed package identifier " ++ s) Nothing

getSpecifiedCabals :: [PackageIdentifier] -> [GenericPackageDescription] -> [GenericPackageDescription]
getSpecifiedCabals list packages = filter wasSpecified packages
  where set = Set.fromList list
        wasSpecified p = Set.member (packageId p) set
