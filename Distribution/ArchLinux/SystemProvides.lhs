Module    : Distribution.ArchLinux.SystemProvides
Copyright : (c) Rémy Oudompheng 2010
License   : BSD3

Maintainer: Arch Haskell Team <arch-haskell@haskell.org>

> module Distribution.ArchLinux.SystemProvides
>  ( SystemProvides(..)
>  , emptySystemProvides
>  , parseSystemProvides
>  ) where

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
>   , platformPackages :: [Dependency]
>      -- ^
>      -- A list of packages to preferably use (e.g. Haskell Platform)
>   , translationTable :: M.Map String String
>      -- ^
>      -- A hash-map where keys are library names and values are
>      -- names of the corresponding ArchLinux package.
>   }
>   deriving (Show,Eq)

Empty SystemProvides

> emptySystemProvides = SystemProvides [] [] M.empty

Get SystemProvides from files.

> parseSystemProvides :: String -> String -> String -> SystemProvides
> parseSystemProvides sPkg sPlat sTranslation =
>          SystemProvides { corePackages = parseDeplist sPkg
>                         , platformPackages = parseDeplist sPlat
>                         , translationTable = parseTranslationTable sTranslation }

Extract a list of dependency descriptions from a file

> depstr2hs :: String -> Maybe Dependency
> depstr2hs s | s == "" || head s == '#' = Nothing
>             | otherwise = simpleParse s

> parseDeplist :: String -> [Dependency]
> parseDeplist srcfile = mapMaybe depstr2hs $ lines srcfile

Now we translate the "library-providers" file. Any line beginning with "# "
or lines with something else than two words are discarded. Lines should have
the form "libraryname packagename".

> trstr2hs :: String -> Maybe (String, String)
> trstr2hs s = case words s of
>   "#":_ -> Nothing
>   a:b:_ -> Just (a,b)
>   _ -> Nothing

> parseTranslationTable :: String -> M.Map String String
> parseTranslationTable srcfile2 = M.fromList $ mapMaybe trstr2hs $ lines srcfile2
