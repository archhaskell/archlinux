{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module    : Distribution.ArchLinux.PkgBuild
-- Copyright : (c) Don Stewart, 2008-2010
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons@galois.com>
--

module Distribution.ArchLinux.PkgBuild (
         PkgBuild(..),
         emptyPkgBuild,
         AnnotatedPkgBuild(..),
         emptyPkg,
         ArchList(..),
         ArchDep(..),
         pkgnameFromArchDep,
         decodePackage,
         pkg2doc
       ) where

import Distribution.Text
import Distribution.Version
import Distribution.PackageDescription
import Distribution.Package
import Distribution.License

import Text.PrettyPrint
import Data.List
import Data.Monoid
import Debug.Trace

import Control.Monad
import Control.Monad.Instances
import Data.Char


--
-- | A data type to represent PKGBUILD files
--
data PkgBuild =
  PkgBuild
    { arch_pkgname :: String
        -- ^
        -- The name of the package. This has be a unix-friendly name
        -- as it will be used in the package filename.
    , arch_pkgver  :: Version
        -- ^ The version of the software as released from the authorii
        --  (e.g. ´2.7.1´).
    , arch_pkgrel  :: !Int
        -- ^
        --  This is the release number specific to the Arch Linux
        -- release. This allows package maintainers to make updates to
        -- the package´s configure flags, for example. A pkgrel of 1
        -- is typically used for each upstream software release and is
        -- incremented for intermediate PKGBUILD updates.
    , arch_pkgdesc :: String
        -- ^
        -- This should be a brief description of the package and its
        -- functionality. Try to keep the description to one line of text.
    , arch_arch    :: ArchList ArchArch
        -- ^
        -- Defines on which architectures the given package is
        -- available (e.g. arch=(´i686´ ´x86_64´)).
    , arch_url     :: String
        -- ^
        -- This field contains a URL that is associated with the software
        -- being packaged. This is typically the project´s website.
    , arch_license :: ArchList License
        -- ^
        -- This field specifies the license(s) that apply to the package.
        -- Commonly-used licenses are found in /usr/share/licenses/common. If
        -- you see the package´s license there, simply reference it in the
        -- license field (e.g.  license=(´GPL´)). If the package provides a
        -- license not found in /usr/share/licenses/common, then you should
        -- include the license in the package itself and set
        -- license=(´custom´) or license=(´custom:LicenseName´). The license
        -- should be placed in $pkgdir/usr/share/licenses/$pkgname when
        -- building the package. If multiple licenses are applicable for a
        -- package, list all of them: license=(´GPL´ ´FDL´).
    , arch_makedepends :: ArchList ArchDep
        -- ^
        -- An array of packages that this package depends on to build, but are
        -- not needed at runtime. Packages in this list follow the same format
        -- as depends.

    , arch_depends     :: ArchList ArchDep
        -- ^
        -- An array of packages that this package depends on to run. Packages
        -- in this list should be surrounded with single quotes and contain at
        -- least the package name. Entries can also include a version
        -- requirement of the form name<>version, where <> is one of five
        -- comparisons: >= (greater than or equal to), <= (less than or equal
        -- to), = (equal to), > (greater than), or < (less than).
    , arch_source      :: ArchList String
        -- ^
        -- An array of source files required to build the package. Source
        -- files must either reside in the same directory as the PKGBUILD
        -- file, or be a fully-qualified URL that makepkg will use to download
        -- the file. In order to make the PKGBUILD as useful as possible, use
        -- the $pkgname and $pkgver variables if possible when specifying the
        -- download location. Any files that are compressed will automatically
        -- be extracted, unless found in the noextract array listed below.
    , arch_md5sum      :: ArchList String
        -- ^
        -- This array contains an MD5 hash for every source file specified in
        -- the source array (in the same order). makepkg will use this to
        -- verify source file integrity during subsequent builds. To easily
        -- generate md5sums, run “makepkg -g >> PKGBUILD”. If desired, move
        -- the md5sums line to an appropriate location.  NOTE: makepkg
        -- supports multiple integrity algorithms and their corresponding
        -- arrays (i.e. sha1sums for the SHA1 algorithm); however, official
        -- packages use only md5sums for the time being.
    , arch_build        :: [String]
        -- ^
        -- The build hook
    , arch_package        :: [String]
        -- ^
        -- The packaging hook
    , arch_install      :: Maybe String
        -- ^
        -- Specifies a special install script that is to be included in the package. This
        -- file should reside in the same directory as the PKGBUILD, and will be copied
        -- into the package by makepkg. It does not need to be included in the source
        -- array (e.g.  install=pkgname.install).
            --
    , arch_options      :: ArchList ArchOptions
        -- ^
        -- This array allows you to override some of makepkg´s default behavior when
        -- building packages. To set an option, just include the option name in the
        -- options array. To reverse the default behavior, place an “!” at the front of
        -- the option. Only specify the options you specifically want to override, the
        -- rest will be taken from makepkg.conf(5).  NOTE: force is a special option only
        -- used in a PKGBUILD(5), do not use it unless you know what you are doing.

    }
    deriving (Show, Eq)

data ArchOptions
    = Strip
    deriving (Show, Eq)

--
-- | An empty PKGBUILD
--
emptyPkgBuild :: PkgBuild
emptyPkgBuild =
  PkgBuild
    { arch_pkgname     = display $ pkgName (package e)
    , arch_pkgver      = pkgVersion (package e)
    , arch_pkgrel      = 1
    , arch_pkgdesc     = synopsis e
    , arch_arch        = ArchList [Arch_X86, Arch_X86_64]
    , arch_url         = homepage e
    , arch_license     = ArchList [license e]
    , arch_depends     = ArchList []
    , arch_makedepends = ArchList []
    , arch_source      = ArchList []
    , arch_md5sum      = ArchList []
        -- sha1sums=('a08670e4c749850714205f425cb460ed5a0a56b2')
    , arch_build       = []
    , arch_package     = []
    , arch_install     = Nothing  -- executable
    , arch_options     = ArchList [Strip]
    }
  where
    e = emptyPackageDescription

------------------------------------------------------------------------
-- Extra pretty printer instances and types

newtype ArchDep = ArchDep Dependency
  deriving (Eq,Show)

instance Text ArchOptions where
  disp Strip = text "strip"
  parse = undefined

-- the PKGBUILD version spec is less expressive than cabal, we can
-- only handle simple intervals like (0,v) or (v,+infty)
mydisp :: VersionInterval -> Doc
mydisp (LowerBound v InclusiveBound, NoUpperBound) = if v==zeroVersion then empty else text ">=" <> disp v
mydisp (LowerBound v ExclusiveBound, NoUpperBound) = text ">" <> disp v
mydisp (_, UpperBound v boundType) = text symbol <> disp v
  where symbol = if boundType == InclusiveBound then "<=" else "<"

zeroVersion :: Version
zeroVersion = Version [0] []

instance Text ArchDep where
  disp (ArchDep (Dependency name ver)) =
    disp name <> mydisp2 intervals
   where
      intervals = asVersionIntervals ver
      strName = display name
     --  >= (greater than or equal to), <= (less than or
     --  equal to), = (equal to), > (greater than), or <
      mydisp2 l | l == []      = trace ("WARNING: version requirement for " ++
                                   strName ++ " is logically impossible.") empty
                | tail l == [] = mydisp $ head l
                -- If there are multiple possible ranges, take only latest versions
                | otherwise    = trace ("WARNING: multiple version ranges specified for " ++
                                        strName ++ " using only the last one.") $ mydisp $ last l

  parse = undefined

--
-- | Extract just the package name from ArchDep
--
pkgnameFromArchDep :: ArchDep -> String
pkgnameFromArchDep (ArchDep (Dependency (PackageName p) _)) = p

--
-- | Valid linux platforms
--
data ArchArch = Arch_X86 | Arch_X86_64
    deriving (Show, Eq)

instance Text ArchArch where
    disp x = case x of
       Arch_X86      -> text "i686"
       Arch_X86_64   -> text "x86_64"
    parse = error "Text.parrse not defined for ArchList"

-- Lists with quotes
newtype ArchList a = ArchList [a]
  deriving (Show, Eq, Monoid, Functor)

instance Text String where
    disp s = text s
    parse = error "Text.parse not defined for String"

instance Text a => Text (ArchList a) where
    disp (ArchList xs) =
         parens (hcat
                (intersperse space
                    (map (quotes . disp) xs)))
    parse = error "Text.parse not defined for ArchList"

-- | Printing with no quotes
dispNoQuotes :: Text a => ArchList a -> Doc
dispNoQuotes (ArchList xs) =
         parens (hcat
                (intersperse space
                    (map disp xs)))


------------------------------------------------------------------------
-- Support for parsing PKGBULIDs

-- | A PKGBUILD data structure with additional metadata
data AnnotatedPkgBuild =
     AnnotatedPkgBuild
        {pkgBuiltWith :: Maybe Version   -- ^ version of cabal2arch used, if any
        ,pkgHeader    :: String          -- ^ header strings
        ,hkgName      :: String          -- ^ package name on Hackage
        ,pkgBody      :: PkgBuild }      -- ^ contents of pkgbuild file
    deriving (Eq, Show)

-- | Empty state structure
emptyPkg :: AnnotatedPkgBuild
emptyPkg = AnnotatedPkgBuild
    { pkgBuiltWith = Nothing
    , pkgHeader    = []
    , hkgName      = []
    , pkgBody      = emptyPkgBuild { arch_options = ArchList []
                                , arch_makedepends = ArchList []
                                }
    }

-- | Result type for pkgbuild parsers
type ResultP a = Either String a

decodePackage :: String -> ResultP AnnotatedPkgBuild
decodePackage s = runGetPKG (readPackage emptyPkg) s

-- | The type of pkgbuild parsers for String
newtype GetPKG a = GetPKG { un :: String -> Either String (a,String) }

instance Functor GetPKG where fmap = liftM

instance Monad GetPKG where
  return x       = GetPKG (\s -> Right (x,s))
  fail x         = GetPKG (\_ -> Left x)
  GetPKG m >>= f = GetPKG (\s -> case m s of
                                     Left err -> Left err
                                     Right (a,s1) -> un (f a) s1)

------------------------------------------------------------------------

-- | Run a PKG reader on an input String, returning a PKGBUILD
runGetPKG :: GetPKG a -> String -> ResultP a
runGetPKG (GetPKG m) s = case m s of
     Left err    -> Left err
     Right (a,t) -> case t of
                        [] -> Right a
                        _  -> Left $ "Invalid tokens at end of PKG string: "++ show (take 10 t)

getInput   :: GetPKG String
getInput    = GetPKG (\s -> Right (s,s))

setInput   :: String -> GetPKG ()
setInput s  = GetPKG (\_ -> Right ((),s))

-- 2010-10-29: This code is unused. Do we still need it?
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- x <$> y = fmap x y

------------------------------------------------------------------------

-- read until end of line
line :: String -> GetPKG String
line s = case break (== '\n') s of
    (h , _ : rest) -> do
        setInput rest
        return h

-- | Recursively parse the pkgbuild
--
readPackage :: AnnotatedPkgBuild -> GetPKG AnnotatedPkgBuild
readPackage st = do
  cs <- getInput

  case cs of
    _ | "# Maintainer"       `isPrefixOf` cs -> do
            h <- line cs
            readPackage st { pkgHeader = h }

      | "# Package generated" `isPrefixOf` cs -> do
            h <- line cs
            let v = simpleParse
                    . reverse
                    . takeWhile (not . isSpace)
                    . reverse $ h
            readPackage st { pkgBuiltWith = v }

      | "_hkgname="  `isPrefixOf` cs -> do
            h <- line cs
            let s = drop 9 h
            readPackage st { hkgName = s }

      | "pkgname="  `isPrefixOf` cs -> do
            h <- line cs
            let s = drop 8 h
            readPackage st { pkgBody = (pkgBody st) { arch_pkgname = s } }

      | "pkgrel="   `isPrefixOf` cs -> do
            h <- line cs
            let s = drop 7 h
            readPackage st { pkgBody = (pkgBody st) { arch_pkgrel = read s } }

      | "pkgver="   `isPrefixOf` cs -> do
            h <- line cs
            let s = drop 7 h
            case simpleParse s of
                Nothing -> fail $ "Unable to parse package version"
                Just v  -> readPackage st { pkgBody = (pkgBody st) { arch_pkgver = v } }

      | "pkgdesc="  `isPrefixOf` cs -> do
            h <- line cs
            let s = drop 8 h
            readPackage st { pkgBody = (pkgBody st) { arch_pkgdesc = s } }

      | "url="      `isPrefixOf` cs -> do
            h <- line cs
            let s = drop 4 h
            readPackage st { pkgBody = (pkgBody st) { arch_url = s } }

      | "license="  `isPrefixOf` cs -> do
            h <- line cs
            let s = takeWhile (/= '\'')
                    . drop 1
                    . dropWhile (/= '\'')
                    . drop 8 $ h
                s' | "custom:" `isPrefixOf` s = drop 7 s
                   | otherwise                = s

            case simpleParse s' of
                Nothing -> readPackage st { pkgBody = (pkgBody st) { arch_license = ArchList [UnknownLicense s'] } }
                Just l  -> readPackage st { pkgBody = (pkgBody st) { arch_license = ArchList [l] } }

      | "depends=("  `isPrefixOf` cs -> do
            h <- line cs
            let s = drop 9 h
            readPackage st { pkgBody = (pkgBody st) { arch_depends = readDepends s } }

      | "makedepends=("  `isPrefixOf` cs -> do
            h <- line cs
            let s = drop 13 h
            readPackage st { pkgBody = (pkgBody st) { arch_makedepends = readDepends s } }

    -- do these later:

      | "arch="     `isPrefixOf` cs
            -> do _ <- line cs ; readPackage st
      | "options="  `isPrefixOf` cs
            -> do _ <- line cs ; readPackage st
      | "source="   `isPrefixOf` cs
            -> do _ <- line cs ; readPackage st
      | "install="  `isPrefixOf` cs
            -> do _ <- line cs ; readPackage st
      | "md5sums="  `isPrefixOf` cs
            -> do _ <- line cs ; readPackage st
      | "build()"   `isPrefixOf` cs
            -> do setInput [] ; return st
      | "package()"  `isPrefixOf` cs
            -> do setInput [] ; return st

    -- skip comments
      | "#" `isPrefixOf` cs
            -> do _ <- line cs ; readPackage st

      | otherwise -> fail $ "Malformed PKGBUILD: " ++ take 80 cs

--
-- | Read a quoted list of depends
--
readDepends :: String -> ArchList ArchDep
readDepends s =
  let s1 = dropWhile (\x -> x `elem` "' )") s
    in case s1 of
      "" -> ArchList []
      _ -> ArchList (d:ds)
        where dep = takeWhile (\x -> x `notElem` "' ") s1
              -- end of the dep field
              s2 = dropWhile (\x -> x `notElem` "' ") s1
              s3 = dropWhile (\x -> x `elem` "' ") s2
              d = str2archdep dep
              ArchList ds = readDepends s3

-- TODO : read version spec
str2archdep :: String -> ArchDep
str2archdep s = case v of
    Nothing -> ArchDep (Dependency (PackageName name) anyVersion)
    Just w -> ArchDep (Dependency (PackageName name) w)
  where name = takeWhile (\x -> x `notElem` "<=>") s
        vspec = dropWhile (\x -> x `notElem` "<=>") s
        v = simpleParse vspec

------------------------------------------------------------------------
-- Pretty printing:

--
-- | Translate an abstract PkgBuild file into a document structure
--

(<=>) :: Doc -> Doc -> Doc
x <=> y = x <> char '=' <> y

--
-- Print a PKGBUILD without comments
--
rawpkg2doc :: PkgBuild -> Doc
rawpkg2doc pkg = vcat
 [ text "pkgname"
    <=> text (arch_pkgname pkg)
 , text "pkgver"
    <=> disp (arch_pkgver pkg)
 , text "pkgrel"
    <=> int (arch_pkgrel pkg)
 , text "pkgdesc"
    <=> doubleQuotes (text $ escapeForBash $ arch_pkgdesc pkg)
 , text "url"
    <=> doubleQuotes (text (arch_url pkg))
 , text "license"
    <=> disp (arch_license pkg)
 , text "arch"
    <=> disp (arch_arch pkg)
 , text "makedepends"
    <=> disp (arch_makedepends pkg)
 , case arch_depends pkg of
        ArchList [] -> empty
        _           -> text "depends" <=> disp (arch_depends pkg)
 , text "options" <=> disp (arch_options pkg)
 , text "source"
    <=> dispNoQuotes (arch_source pkg)
 , case arch_install pkg of
    Nothing -> empty
    Just p  -> text "install" <=> disp p
 , text "md5sums"
    <=> disp (arch_md5sum pkg)
 , hang
    (text "build() {") 4
             (vcat $ (map text) (arch_build pkg))
   $$ char '}'
 , hang
    (text "package() {") 4
             (vcat $ (map text) (arch_package pkg))
   $$ char '}'
 ]

--
-- | Helper function to escape strings for PKGBUILDs
--
escapeForBash :: String -> String
escapeForBash = concatMap escapeCharForBash

escapeCharForBash c = case c of
 '$'  ->  "\\$"
 '`'  -> "\\`"
 '"'  -> "\\\""
 '\\' -> "\\\\"
 '\n' -> " "
 x    -> [x]

instance Text PkgBuild where
  disp p = rawpkg2doc p
  parse = undefined

--
-- Display a PKGBUILD with header
--
instance Text AnnotatedPkgBuild where
  disp AnnotatedPkgBuild {
    pkgBuiltWith = ver,
    pkgHeader = header,
    hkgName = hkg,
    pkgBody = pkg
  } = vcat [ if null header then empty else text header
           , text "_hkgname" <=> text hkg
           , disp pkg ]
  parse = undefined

--
-- Display a full PKGBUILD with Maintainer name.
--
pkg2doc :: String -> AnnotatedPkgBuild -> Doc
pkg2doc email pkg = text "# Maintainer:" <+> text email $$ disp pkg

--
-- | A data type to represent a full ArchLinux package
--
data ArchPackage = ArchPackage
  { archpkg_pkgbuild :: AnnotatedPkgBuild
        -- ^
        -- The annotated PKGBUILD file
  , archpkg_install :: Maybe String
        -- ^
        -- The contents of the install script if it exists.
  , archpkg_others :: [(String, String)]
        -- ^
        -- A list of additional files (filename, contents)
  }

--
-- | An empty package
--
emptyArchPkg :: ArchPackage
emptyArchPkg = ArchPackage
  { archpkg_pkgbuild = emptyPkg
  , archpkg_install = Nothing
  , archpkg_others = []
  }

