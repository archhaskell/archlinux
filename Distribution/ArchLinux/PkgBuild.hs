{-# LANGUAGE TypeSynonymInstances        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- |
-- Module    : Distribution.ArchLinux.PkgBuild
-- Copyright : (c) Don Stewart, 2008-2009
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons@galois.com>
--

module Distribution.ArchLinux.PkgBuild where

import Distribution.Text
import Distribution.Version
import Distribution.PackageDescription
import Distribution.Package
import Distribution.License

import Text.PrettyPrint
import Data.List
import Data.Monoid
import Debug.Trace

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

    -- everything depends on ghc and Cabal 1.4.x
    , arch_makedepends = ArchList
        [(ArchDep (Dependency (PackageName "ghc")    AnyVersion))
        ,(ArchDep (Dependency (PackageName "haskell-cabal") AnyVersion))
--        ,(ArchDep (Dependency "haskell-cabal" (LaterVersion (Version  [1,4,0,0] []))))
        ]

        -- makedepends=('ghc>=6.6') ?
    , arch_depends     = ArchList []
    , arch_source      = ArchList []
    , arch_md5sum      = ArchList []
        -- sha1sums=('a08670e4c749850714205f425cb460ed5a0a56b2')
    , arch_build       = []
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

-- the PKGBUILD version spec is less expressive than cabal, we can't
-- really handle unions or intersections well yet.

instance Text ArchDep where
  disp (ArchDep (Dependency name ver)) =
    text (display name) <> mydisp ver
   where
     --  >= (greater than or equal to), <= (less than or
     --  equal to), = (equal to), > (greater than), or <
      mydisp AnyVersion           = empty

      mydisp (ThisVersion    v)   = text "=" <> disp v
      mydisp (LaterVersion   v)   = char '>' <> disp v
      mydisp (EarlierVersion v)   = char '<' <> disp v

      mydisp (UnionVersionRanges (ThisVersion  v1) (LaterVersion v2))
        | v1 == v2 = text ">=" <> disp v1
      mydisp (UnionVersionRanges (LaterVersion v2) (ThisVersion  v1))
        | v1 == v2 = text ">=" <> disp v1
      mydisp (UnionVersionRanges (ThisVersion v1) (EarlierVersion v2))
        | v1 == v2 = text "<=" <> disp v1
      mydisp (UnionVersionRanges (EarlierVersion v2) (ThisVersion v1))
        | v1 == v2 = text "<=" <> disp v1

{-
      mydisp (UnionVersionRanges r1 r2)
        = disp r1 <+> text "||" <+> disp r2

      mydisp (IntersectVersionRanges r1 r2)
        = disp r1 <+> text "&&" <+> disp r2
-}

      mydisp x = trace ("WARNING: Can't handle this version format yet: " ++ show x ++ "\ncheck the dependencies by hand.")$ empty

  parse = undefined

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


