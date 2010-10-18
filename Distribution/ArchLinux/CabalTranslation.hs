-- |
-- Module    : Distribution.ArchLinux.CabalTranslation
-- Copyright : (c) Don Stewart, 2008-2010, Rémy Oudompheng 2010
-- License   : BSD3
--
-- Maintainer: Arch Haskell Team <arch-haskell@haskell.org>
--

module Distribution.ArchLinux.CabalTranslation (
         preprocessCabal,
         cabal2pkg,
         oldCabal2Arch,
         install_hook_name
       ) where
-- Cabal modules
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration
import Distribution.License
import Distribution.Version
import Distribution.Compiler
import Distribution.System
-- Archlinux modules
import Distribution.ArchLinux.PkgBuild
-- Standard types
import Distribution.Text
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import System.FilePath
-- Debugging
import Debug.Trace

--
-- | Configure package for system
--
preprocessCabal :: GenericPackageDescription -> Maybe PackageDescription
preprocessCabal cabalsrc =
     case finalizePackageDescription
        []
        (const True) -- could check against prefered pkgs....
        (Platform X86_64 buildOS) -- linux/x86_64
        (CompilerId GHC (Version [6,10,3] []))

        -- now constrain it to solve in the context of a modern ghc only
        corePackages
        cabalsrc
     of
        Left deps     -> trace ("Unresolved dependencies: " ++show deps) Nothing
        Right (pkg,_) -> Just pkg { buildDepends = removeCoreFrom (buildDepends pkg) }

-- attempt to filter out core packages we've already satisified
-- not actuall correct, since it doesn't take any version
-- info into account.
--
-- TODO this should use configDependency to find the precise
-- versions we have available on Arch.
--
removeCoreFrom :: [Dependency] -> [Dependency]
removeCoreFrom []               = []
removeCoreFrom (x@(Dependency n vr):xs) =
  case find (\(Dependency k _) -> n == k) corePackages of
    -- haskell-parsec, haskell-quickcheck
    Just (Dependency _ (ThisVersion v'))
        | withinRange v' vr         ->     removeCoreFrom xs

    Just (Dependency (PackageName "base") _)
                                    ->     removeCoreFrom xs

    Just (Dependency _ AnyVersion)  ->     removeCoreFrom xs
    _                               -> x : removeCoreFrom xs

--
-- Core packages and their versions. These come with
-- ghc, so we should be right.
--
-- http://haskell.org/haskellwiki/Libraries_released_with_GHC
--
-- And what Arch Linux thinks GHC provides:
--
-- http://repos.archlinux.org/wsvn/packages/ghc/repos/extra-x86_64/PKGBUILD
--
-- Note: we could just list these directly, and have yaourt solve them.
--
-- NEW POLICY:
--      We rely on all "provides" from the GHC library to be listed explicitly.
--
corePackages :: [Dependency]
corePackages =
    [

-- Magic packages we have to remove
     Dependency (PackageName "base")             (ThisVersion (Version  [4,1,0,0] []))
    ,Dependency (PackageName "dph-base")           (AnyVersion)
    ,Dependency (PackageName "dph-par" )           (AnyVersion)
    ,Dependency (PackageName "dph-prim-interface") (AnyVersion)
    ,Dependency (PackageName "dph-prim-par"   )    (AnyVersion)
    ,Dependency (PackageName "dph-prim-seq"   )    (AnyVersion)
    ,Dependency (PackageName "dph-seq"        )    (AnyVersion)
    ,Dependency (PackageName "ghc")              (AnyVersion)
    ,Dependency (PackageName "ghc-prim")         (AnyVersion)
    ,Dependency (PackageName "integer")         (AnyVersion)
    ,Dependency (PackageName "integer-gmp")         (AnyVersion)
    ,Dependency (PackageName "ghc-binary")         (AnyVersion)

-- Official Provides: http://repos.archlinux.org/wsvn/packages/ghc/repos/extra-x86_64/PKGBUILD
--  ,Dependency (PackageName "array")            (ThisVersion (Version  [0,3,0,0] []))
--  ,Dependency (PackageName "bytestring")       (ThisVersion (Version  [0,9,1,5] []))
--  ,Dependency (PackageName "Cabal")            (ThisVersion (Version  [1,8,0,2] []))
--  ,Dependency (PackageName "containers")       (ThisVersion (Version  [0,3,0,0] []))
--  ,Dependency (PackageName "directory")        (ThisVersion (Version  [1,0,1,0] []))
--  ,Dependency (PackageName "extensible-exceptions")         (AnyVersion)
--  ,Dependency (PackageName "filepath")         (ThisVersion (Version  [1,1,0,3] []))
--  ,Dependency (PackageName "haskell98")        (ThisVersion (Version  [1,0,1,1] []))
--  ,Dependency (PackageName "hpc")              (ThisVersion (Version  [0,5,0,4] []))
--  ,Dependency (PackageName "old-locale")       (ThisVersion (Version  [1,0,0,2] []))
--  ,Dependency (PackageName "old-time")         (ThisVersion (Version  [1,0,0,1] []))
--  ,Dependency (PackageName "pretty")           (ThisVersion (Version  [1,0,1,1] []))
--  ,Dependency (PackageName "process")          (ThisVersion (Version  [1,0,1,2] []))
--  ,Dependency (PackageName "random")           (ThisVersion (Version  [1,0,0,2] []))
--  ,Dependency (PackageName "syb")              (ThisVersion (Version  [0,1,0,2] []))
--  ,Dependency (PackageName "template-haskell") (ThisVersion (Version  [2,4,0,0] []))
--  ,Dependency (PackageName "time")             (ThisVersion (Version  [1,1,4] []))
--  ,Dependency (PackageName "unix")             (ThisVersion (Version  [2,4,0,0] []))
--  utf8-string


-- Removed in 6.12.x
--  ,Dependency (PackageName "html")             (ThisVersion (Version  [1,0,1,2] []))
--  ,Dependency (PackageName "integer")          (ThisVersion (Version  [0,1,0,0] []))
--  ,Dependency (PackageName "QuickCheck")       (ThisVersion (Version  [1,2,0,0] []))
--  ,Dependency (PackageName "haskell-src")      (ThisVersion (Version  [1,0,1,3] []))
--  ,Dependency (PackageName "parsec")           (ThisVersion (Version  [2,1,0,0] []))
--  ,Dependency (PackageName "packedstring")     (ThisVersion (Version  [0,1,0,1] []))
--  ,Dependency (PackageName "parallel")         (ThisVersion (Version  [1,1,0,0] []))
--  ,Dependency (PackageName "network")          (ThisVersion (Version  [2,2,0,1] []))
--  ,Dependency (PackageName "mtl")              (ThisVersion (Version  [1,1,0,2] []))
--  ,Dependency (PackageName "stm")              (ThisVersion (Version  [2,1,1,2] []))
--  ,Dependency (PackageName "HUnit")            (ThisVersion (Version  [1,2,0,3] []))
--  ,Dependency (PackageName "xhtml")            (ThisVersion (Version  [3000,2,0,1] []))
--  ,Dependency (PackageName "regex-base")       (ThisVersion (Version  [0,72,0,2] []))
--  ,Dependency (PackageName "regex-compat")     (ThisVersion (Version  [0,71,0,1] []))
--  ,Dependency (PackageName "regex-posix")      (ThisVersion (Version  [0,72,0,2] []))

-- Removed in 6.10.x
--  ,Dependency (PackageName "editline")         (AnyVersion)
--   Dependency (PackageName "ALUT")             (ThisVersion (Version  [2,1,0,0] []))
--  ,Dependency (PackageName "cgi")              (ThisVersion (Version  [3001,1,5,1] []))
--  ,Dependency (PackageName "fgl")              (ThisVersion (Version  [5,4,1,1] [])) -- gone
--  ,Dependency (PackageName "GLUT")             (ThisVersion (Version  [2,1,1,1] []))
--  ,Dependency (PackageName "OpenAL")           (ThisVersion (Version  [1,3,1,1] [])) -- gone
--  ,Dependency (PackageName "readline")         (ThisVersion (Version  [1,0,1,0] []))

    ]

------------------------------------------------------------------------------------

--
-- | Translate a generic cabal file into a PGKBUILD
--
cabal2pkg :: PackageDescription -> (PkgBuild, Maybe String)
cabal2pkg cabal

-- TODO decide if it's a library or an executable,
-- handle multipackages
-- extract C dependencies

-- = trace (show cabal) $
  =
  (emptyPkgBuild
    { arch_pkgname = archName
    , arch_pkgver  = vers
    , arch_url     = "http://hackage.haskell.org/package/"++display name
 --       else homepage cabal
    , arch_pkgdesc = case synopsis cabal of
                          [] -> take 80 (description cabal)
                          s  -> s
    , arch_license =
        ArchList . return $
            case license cabal of
                x@GPL {} -> x
                x@LGPL {} -> x
                l    -> UnknownLicense ("custom:"++ show l)

    -- All Hackage packages depend on GHC at build time
    -- All Haskell libraries are prefixed with "haskell-"
    , arch_makedepends = if not hasLibrary
            then my_makedepends
            else ArchList [] -- makedepends should not duplicate depends

    , arch_depends =
        (if not (isLibrary)
            then
                ArchList [ArchDep (Dependency (PackageName "gmp") AnyVersion)]
                                `mappend`
                                anyClibraries
            else ArchList [])
        `mappend`
            -- libraries have 'register-time' dependencies on
            -- their dependent Haskell libraries.
            --
           (if hasLibrary then my_makedepends
                          else ArchList [])

    -- need the dependencies of all flags that are on by default, for all libraries and executables

    -- Hackage programs only need their own source to build
    , arch_source  = ArchList . return $
          "http://hackage.haskell.org/packages/archive/"
       ++ (display name </> display vers </> display name <-> display vers <.> "tar.gz")

    , arch_build =
        [ "cd ${srcdir}/" </> display name <-> display vers
        , "runhaskell Setup configure --prefix=/usr --docdir=/usr/share/doc/${pkgname}"
        , "runhaskell Setup build"
        ] ++

    -- Only needed for libraries:
        (if hasLibrary
           then
            [ "runhaskell Setup haddock"
            , "runhaskell Setup register   --gen-script"
            , "runhaskell Setup unregister --gen-script"
            ]
           else [])

    , arch_package =
        [ "cd ${srcdir}/" </> display name <-> display vers ] ++
        -- Only needed for libraries:
        (if hasLibrary
           then
            [ "install -D -m744 register.sh   ${pkgdir}/usr/share/haskell/${pkgname}/register.sh"
            , "install    -m744 unregister.sh ${pkgdir}/usr/share/haskell/${pkgname}/unregister.sh"
            , "install -d -m755 ${pkgdir}/usr/share/doc/ghc/html/libraries"
            , "ln -s /usr/share/doc/${pkgname}/html ${pkgdir}/usr/share/doc/ghc/html/libraries/" ++ (display name)
            ]
           else [])
         ++
         ["runhaskell Setup copy --destdir=${pkgdir}"]
         ++
         (if not (null (licenseFile cabal)) && (case license cabal of GPL {} -> False; LGPL {} -> False; _ -> True)
          then
              [ "install -D -m644 " ++ licenseFile cabal ++ " ${pkgdir}/usr/share/licenses/${pkgname}/LICENSE"
              , "rm -f ${pkgdir}/usr/share/doc/${pkgname}/LICENSE"
              ]
          else [])

    -- if its a library:
    , arch_install = if hasLibrary then Just $ install_hook_name archName
                                   else Nothing

    }, if hasLibrary
          then Just (install_hook archName)
          else Nothing
    )

  where
    archName = map toLower (if isLibrary then "haskell-" ++ display name else display name)
    name     = pkgName (package cabal)
    vers     = pkgVersion (package cabal)

    -- build time dependencies
    my_makedepends =
     (arch_makedepends emptyPkgBuild)
        `mappend`
     -- Haskell libraries
     -- TODO: use a real package spec to compute these names
     -- based on what is in Arch.
     ArchList
         [ ArchDep (Dependency (PackageName $
               if d `notElem` shouldNotBeLibraries
                    then "haskell" <-> map toLower (display d) else display d) v)
         | Dependency (PackageName d) v <- gtk2hsIfy (buildDepends cabal) ]
        `mappend`
     anyClibraries
        `mappend`
     ArchList [ ArchDep d' | b <- allBuildInfo cabal
                          , d@(Dependency n _) <- buildTools b
                          , n /= PackageName "hsc2hs"
                          , let d' | n `elem` gtkTools
                                        = Dependency (PackageName "gtk2hs-buildtools") AnyVersion
                                   | otherwise        = d
                          ]

    gtkTools = map PackageName ["gtk2hsTypeGen" , "gtk2hsHookGenerator", "gtk2hsC2hs"]

    -- TODO: need a 'nub' in here.

    hasLibrary = isJust (library cabal)
    isLibrary  = isJust (library cabal) -- && null (executables cabal)
                    && map toLower (display name) `notElem` shouldNotBeLibraries

    anyClibraries | null libs = ArchList []
                  | otherwise = ArchList libs
       where
         libs = [ ArchDep (Dependency (PackageName s) AnyVersion) | s <- nub (findCLibs cabal) ]

(<->) :: String -> String -> String
x <-> y = x ++ "-" ++ y

--
-- post install, and pre-remove hooks to run, to sync up ghc-pkg
--
install_hook_name :: String -> String
install_hook_name pkgname = pkgname <.> "install"

install_hook :: String -> String
install_hook pkgname = unlines
    [ "HS_DIR=/usr/share/haskell/" ++ pkgname
    , "post_install() {"
    , "  ${HS_DIR}/register.sh"
    , "  (cd /usr/share/doc/ghc/html/libraries; ./gen_contents_index)"
    , "}"
    , "pre_upgrade() {"
    , "  ${HS_DIR}/unregister.sh"
    , "}"
    , "post_upgrade() {"
    , "  ${HS_DIR}/register.sh"
    , "  (cd /usr/share/doc/ghc/html/libraries; ./gen_contents_index)"
    , "}"
    , "pre_remove() {"
    , "  ${HS_DIR}/unregister.sh"
    , "}"
    , "post_remove() {"
    , "  (cd /usr/share/doc/ghc/html/libraries; ./gen_contents_index)"
    , "}"
    , "op=$1"
    , "shift"
    , "$op $*" ]

findCLibs :: PackageDescription -> [String]
findCLibs (PackageDescription { library = lib, executables = exe }) =
    -- warn for packages not in list.
    filter (not . null) $ map (canonicalise . map toLower) (some ++ rest)
  where
    some = concatMap (extraLibs.buildInfo) exe
    rest = case lib of
                    Nothing -> []
                    Just l  -> extraLibs (libBuildInfo l) ++
                                map (\(Dependency (PackageName n) _) ->
                                    if '-' `elem` n
                                        then reverse . drop 1 . dropWhile (/= '-') .  reverse $ n
                                        else n)
                                        (pkgconfigDepends (libBuildInfo l))

    canonicalise k = case M.lookup k translationTable of
        Nothing -> trace ("WARNING: this library depends on a C library we do not know the pacman name for (" ++ map toLower k ++ ") . Check the C library names in the generated PKGBUILD File") $ map toLower k
        Just s  -> s

    -- known pacman packages for C libraries we use:
    translationTable = M.fromList
        [("Imlib2",     "imlib2")
        ,("SDL",        "sdl")
        ,("alut",       "freealut")
        ,("bz2",        "bzip2")
        ,("cblas",      "blas")
        ,("crack",      "cracklib")
        ,("crypto",     "openssl")
        ,("curl",       "curl")
        ,("freetype",   "freetype2")
        ,("glib",       "glib2")
        ,("wmflite",    "libwmf")
        ,("il",    "devil")

        ,("jpeg",       "libjpeg")
        ,("ldap",       "libldap")
        ,("pcap",       "libpcap")
        ,("png",        "libpng")
        ,("x11",        "libx11")
        ,("xrandr",     "libxrandr")
        ,("xml2",       "libxml2")
        ,("exif",       "libexif")
        ,("tiff",       "libtiff")
        ,("sndfile",    "libsndfile")
        ,("gcrypt",     "libgcrypt")
        ,("fftw3",      "fftw")

        ,("pq",         "postgresql")
        ,("ssl",        "openssl")
        ,("wx",         "wxgtk")
        ,("xenctrl",    "xen")
        ,("odbc",       "unixodbc")
        ,("z",          "zlib")
        ,("curses",     "ncurses")
        ,("xslt",       "libxslt")
        ,("csound64",   "csound5")
        ,("uuid",       "e2fsprogs")
        ,("doublefann", "fann")
        ,("ev",         "libev")

        ,("pthread",    "")
        ,("m",          "")
        ,("gl",         "")
        ,("glu",        "")
        ,("db_cxx",     "")
        ,("db_cxx",     "")
        ,("xdamage",    "")

        ,("icui18n",          "icu")
        ,("icuuc",          "icu")
        ,("icudata",          "icu")

        ,("netsnmp",        "net-snmp")
        ,("asound",        "alsa-lib")
        ,("ffi",        "libffi")
        ,("ogg",        "libogg")
        ,("theora",        "libtheora")
        ,("mtp",        "libmtp")
        ,("zmq",        "zeromq")
        ,("cv",        "opencv")
        ,("highgui",        "opencv")
        ,("xss",        "libxss")
        ,("idn",        "libidn")
        ,("libgsasl",        "gsasl")
        ,("event",        "libevent")
        ,("gcc_s",        "gcc-libs")


        -- subsumed into glib

        ,("gobject",                "")
        ,("gmodule",                "glib2")
        ,("gio",                    "")
        ,("gthread",                "")
        ,("gnome-vfs-module",       "")
        ,("gstreamer-audio",        "")
        ,("gstreamer-base",         "")
        ,("gstreamer-controller",   "")
        ,("gstreamer-dataprotocol", "")
        ,("gstreamer-net",          "")
        ,("ogremain",          "")
        ,("gnutls-extra",      "gnutls")
        ,("pangocairo",        "pango")

        ,("webkit",      "libwebkit")
        ,("gtk+",        "gtk")
        ,("gdk",         "gtk")
        ,("gdk-x11-2.0", "gtk2")
        ,("gtk-x11-2.0", "gtk2")
        ,("xine",        "xine-lib")
        ,("ncursesw",    "ncurses")
        ,("panel",       "ncurses")

        ,("gstreamer",   "gstreamer0.10")
        ,("gstreamer-plugins-base", "gstreamer0.10-base")
        ]
        -- atlas

shouldNotBeLibraries :: [String]
shouldNotBeLibraries =
    ["xmonad"
    ,"gitit"
    ,"yavie"
    ,"berp"
    ,"l-seed"
    ,"hspresent"
    ,"haskell-platform"
    ,"xmonad-contrib"
    ,"lambdabot"
    ,"piet"
    ,"hsffig"
    ,"yi"
    ,"haddock"
    ,"hscolour"
    ,"line2pdf"
    ,"distract"
    ,"derive"
    ,"Hedi"
    ,"conjure"
    ,"clevercss"
    ,"cpphs"
    ,"backdropper"
    ,"darcs-beta"
    ,"gtk2hs"
    ,"darcs"
    ,"greencard"
-- the pandoc package doesnt' ship haskell-pandoc
--    ,"pandoc"
    ,"pugs-drift"
    ,"wol"
    ,"timepiece"
    ,"hledger"
    ,"hp2any-graph"
    ,"hp2any-manager"
    ]

-- translate some library dependencies to gtk names
--
gtk2hsIfy :: [Dependency] -> [Dependency]
gtk2hsIfy = id

{-
gtk2hsIfy [] = []
gtk2hsIfy xs | foundSome = Dependency (PackageName "gtk2hs") AnyVersion :
                           [ v | v@(Dependency n _) <- xs
                           , n `notElem` gtkLibs ]
             | otherwise = xs

    where
        foundSome = not . null $ filter (`elem` gtkLibs) (map unDep xs)
        unDep (Dependency n _) = n
-}

---------------------------------------------------------------------------

{-
# Contributor: Arch Haskell Team <arch-haskell@haskell.org>
# Package generated by cabal2arch 0.6.1
pkgname=haskell-pcre-light
pkgrel=2
pkgver=0.3.1
pkgdesc="A small, efficient and portable regex library for Perl 5 compatible regular expressions"
url="http://hackage.haskell.org/package/pcre-light"
license=('custom:BSD3')
arch=('i686' 'x86_64')
makedepends=()
depends=('ghc' 'haskell-cabal' 'pcre')
options=('strip')
source=(http://hackage.haskell.org/packages/archive/pcre-light/0.3.1/pcre-light-0.3.1.tar.gz)
install=haskell-pcre-light.install
md5sums=('14d8d6e2fd200c385b1d63c888794014')
build() {
    cd ${srcdir}/pcre-light-0.3.1
    runhaskell Setup configure --prefix=/usr --docdir=/usr/share/doc/${pkgname} || return 1
    runhaskell Setup build                   || return 1
    runhaskell Setup haddock || return 1
    runhaskell Setup register   --gen-script || return 1
    runhaskell Setup unregister --gen-script || return 1
    install -D -m744 register.sh   ${pkgdir}/usr/share/haskell/$pkgname/register.sh
    install    -m744 unregister.sh ${pkgdir}/usr/share/haskell/$pkgname/unregister.sh
    install -d -m755 $pkgdir/usr/share/doc/ghc/libraries
    ln -s /usr/share/doc/${pkgname}/html ${pkgdir}/usr/share/doc/ghc/libraries/pcre-light
    runhaskell Setup copy --destdir=${pkgdir} || return 1
    install -D -m644 LICENSE ${pkgdir}/usr/share/licenses/$pkgname/LICENSE || return 1
    rm -f ${pkgdir}/usr/share/doc/${pkgname}/LICENSE
}
    -}

------------------------------------------------------------------------
-- Checker
--

type Warnings = String

-- Hard code the cabal2arch version
recentCabal2ArchVersion :: Maybe Version
recentCabal2ArchVersion = case simpleParse "0.7" of -- XXX
    Nothing -> error "Unable to parse cabal2arch version"
    Just v  -> Just v

-- | Look for problems in the PKGBUILD
oldCabal2Arch :: AnnotatedPkgBuild -> Bool
oldCabal2Arch s
    | isNothing (pkgBuiltWith s)
    = True

    | pkgBuiltWith s < recentCabal2ArchVersion
    = True -- ["Old version of cabal2arch: " ++ display (fromJust (pkgBuiltWith s))]

    | otherwise                             = False

