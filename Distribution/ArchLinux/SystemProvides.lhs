Module    : Distribution.ArchLinux.SystemProvides
Copyright : (c) Rémy Oudompheng 2010
License   : BSD3

Maintainer: Arch Haskell Team <arch-haskell@haskell.org>

> module Distribution.ArchLinux.SystemProvides where

Cabal modules

> import Distribution.Package

Standard types

> import Distribution.Text
> import qualified Data.Map as M
> import Data.Maybe
> import System.FilePath

> import Paths_archlinux

A big structure holding data about ArchLinux

> data SystemProvides = SystemProvides
>   { corePackages :: [Dependency]
>      -- ^
>      -- A list of Dependencies which are automatically satified
>      -- when GHC is installed.
>   , translationTable :: M.Map String String
>      -- ^
>      -- A hash-map where keys are library names and values are
>      -- names of the corresponding ArchLinux package.
>   }

Get SystemProvides from package-installed files

> getDefaultSystemProvides :: IO SystemProvides
> getDefaultSystemProvides = do
>   fnc <- getDataFileName $ "data" </> "ghc-provides.txt"
>   fnt <- getDataFileName $ "data" </> "library-providers.txt"
>   getSystemProvidesFromFiles fnc fnt

> getSystemProvidesFromFiles :: FilePath -> FilePath -> IO SystemProvides
> getSystemProvidesFromFiles filePkg fileTranslation = do
>   fc <- readFile filePkg
>   ft <- readFile fileTranslation
>   return SystemProvides { corePackages = corePackagesFromFile fc
>                         , translationTable = translationTableFromFile ft }

Extract GHC-provided dependencies from a file

> depstr2hs :: String -> Maybe Dependency
> depstr2hs s | s == "" || head s == '#' = Nothing
>             | otherwise = simpleParse s

> corePackagesFromFile :: String -> [Dependency]
> corePackagesFromFile srcfile1 = mapMaybe depstr2hs $ lines srcfile1

Now we translate the "library-providers" file. Any line beginning with "# "
or lines with something else than two words are discarded. Lines should have
the form "libraryname packagename".

> trstr2hs :: String -> Maybe (String, String)
> trstr2hs s = case words s of
>   "#":_ -> Nothing
>   a:b:_ -> Just (a,b)
>   _ -> Nothing

> translationTableFromFile :: String -> M.Map String String
> translationTableFromFile srcfile2 = M.fromList $ mapMaybe trstr2hs $ lines srcfile2
