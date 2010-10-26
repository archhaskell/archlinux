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
import Distribution.ArchLinux.SystemProvides
-- Standard types
import Distribution.Text
import Text.PrettyPrint
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
preprocessCabal :: GenericPackageDescription -> SystemProvides -> Maybe PackageDescription
preprocessCabal cabalsrc systemContext =
     case finalizePackageDescription
        []
        (const True) -- could check against prefered pkgs....
        (Platform X86_64 buildOS) -- linux/x86_64
        (CompilerId GHC (Version [6,10,3] []))

        -- now constrain it to solve in the context of a modern ghc only
        (corePackages systemContext)
        cabalsrc
     of
        Left deps     -> trace ("Unresolved dependencies: " ++show deps) Nothing
        Right (pkg,_) -> Just pkg { buildDepends = removeCoreFrom (buildDepends pkg) systemContext }

-- attempt to filter out core packages we've already satisified
-- not actuall correct, since it doesn't take any version
-- info into account.
--
-- TODO this should use configDependency to find the precise
-- versions we have available on Arch.
--
removeCoreFrom :: [Dependency] -> SystemProvides -> [Dependency]
removeCoreFrom [] _             = []
removeCoreFrom (x@(Dependency n vr):xs) systemContext =
  case find (\(Dependency k _) -> n == k) $ corePackages systemContext of
    -- haskell-parsec, haskell-quickcheck
    Just (Dependency _ (ThisVersion v'))
        | withinRange v' vr         ->     removeCoreFrom xs systemContext

    Just (Dependency (PackageName "base") _)
                                    ->     removeCoreFrom xs systemContext

    Just (Dependency _ AnyVersion)  ->     removeCoreFrom xs systemContext
    _                               -> x : removeCoreFrom xs systemContext

------------------------------------------------------------------------------------

--
-- | Translate a generic cabal file into a PGKBUILD
--
cabal2pkg :: PackageDescription -> SystemProvides -> (AnnotatedPkgBuild, Maybe String)
cabal2pkg cabal systemContext

-- TODO decide if it's a library or an executable,
-- handle multipackages
-- extract C dependencies

-- = trace (show cabal) $
  = ( emptyPkg {
      pkgHeader = comment
    , hkgName = display name
    , pkgBody = stub {
      arch_pkgname = archName
    , arch_pkgver  = vers
    , arch_pkgdesc = case synopsis cabal of
                          [] -> take 80 (description cabal)
                          s  -> s
    , arch_license =
        ArchList . return $
            case license cabal of
                x@GPL {} -> x
                x@LGPL {} -> x
                l    -> UnknownLicense ("custom:"++ show l)
    , arch_package = (arch_package stub) ++
         (if not (null (licenseFile cabal)) && (case license cabal of GPL {} -> False; LGPL {} -> False; _ -> True)
          then
              [ "install -D -m644 " ++ licenseFile cabal ++ " ${pkgdir}/usr/share/licenses/${pkgname}/LICENSE"
              , "rm -f ${pkgdir}/usr/share/doc/${pkgname}/LICENSE"
              ]
          else []) }
    }, if hasLibrary
          then Just (install_hook archName)
          else Nothing
    )

  where
    stub = if hasLibrary
               then (stubPackageLibrary $ display name) {
                 arch_depends = (
                   if not (isLibrary)
                   then ArchList [ArchDep (Dependency (PackageName "gmp") AnyVersion)]
                        `mappend` anyClibraries
            -- libraries have 'register-time' dependencies on
            -- their dependent Haskell libraries.
            --
                   else ArchList []) `mappend` my_makedepends
                 }
               else (stubPackageProgram $ display name) {
                 -- isLibrary = False automatically
                 arch_makedepends = my_makedepends
               , arch_depends = ArchList [ArchDep (Dependency (PackageName "gmp") AnyVersion)]
                                `mappend` anyClibraries
               }

    archName = map toLower (if isLibrary then "haskell-" ++ display name else display name)
    name     = pkgName (package cabal)
    vers     = pkgVersion (package cabal)

    -- build time dependencies
    my_makedepends =
      -- everything depends on ghc and Cabal 1.4.x
     ArchList
        [(ArchDep (Dependency (PackageName "ghc")    AnyVersion))]
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
         libs = [ ArchDep (Dependency (PackageName s) AnyVersion) | s <- nub (findCLibs cabal systemContext) ]

(<->) :: String -> String -> String
x <-> y = x ++ "-" ++ y

comment :: String
comment = render $ vcat
 [ text "# Note: we list all package dependencies."
 , text "# Your package tool should understand 'provides' syntax"
 , text "#"
 , text "# Keep up to date on http://archhaskell.wordpress.com/"
 , text "#"]

--
-- | A PKGBUILD skeleton for Haskell libraries (hasLibrary = True)
--
stubPackageLibrary :: String -> PkgBuild
stubPackageLibrary hkgname = emptyPkgBuild {
      arch_url     = "http://hackage.haskell.org/package/" ++ hkgname
    -- All Hackage packages depend on GHC at build time
    -- All Haskell libraries are prefixed with "haskell-"
    , arch_makedepends = ArchList [] -- makedepends should not duplicate depends
    -- Hackage programs only need their own source to build
    , arch_source  = ArchList . return $
          "http://hackage.haskell.org/packages/archive/" ++ hkgname ++ "/${pkgver}/" ++ hkgname ++ "-${pkgver}.tar.gz"
    , arch_build =
        [ "cd ${srcdir}/" ++ hkgname ++ "-${pkgver}"
        , "runhaskell Setup configure --prefix=/usr --docdir=/usr/share/doc/${pkgname} -O --enable-split-objs"
        , "runhaskell Setup build"
        , "runhaskell Setup haddock"
        , "runhaskell Setup register   --gen-script"
        , "runhaskell Setup unregister --gen-script"
        ]
    , arch_package =
        [ "cd ${srcdir}/" ++ hkgname ++ "-${pkgver}"
        , "install -D -m744 register.sh   ${pkgdir}/usr/share/haskell/${pkgname}/register.sh"
        , "install    -m744 unregister.sh ${pkgdir}/usr/share/haskell/${pkgname}/unregister.sh"
        , "install -d -m755 ${pkgdir}/usr/share/doc/ghc/html/libraries"
        , "ln -s /usr/share/doc/${pkgname}/html ${pkgdir}/usr/share/doc/ghc/html/libraries/" ++ hkgname
        ,"runhaskell Setup copy --destdir=${pkgdir}"]
    -- if its a library:
    , arch_install = Just "${pkgname}.install"
    }

--
-- | A PKGBUILD skeleton for Haskell programs (hasLibrary = False)
--
stubPackageProgram :: String -> PkgBuild
stubPackageProgram hkgname = emptyPkgBuild {
      arch_url     = "http://hackage.haskell.org/package/" ++ hkgname
    -- Hackage programs only need their own source to build
    , arch_source  = ArchList . return $
          "http://hackage.haskell.org/packages/archive/" ++ hkgname ++ "/${pkgver}/" ++ hkgname ++ "-${pkgver}.tar.gz"
    , arch_build =
        [ "cd ${srcdir}/" ++ hkgname ++ "-${pkgver}"
        , "runhaskell Setup configure --prefix=/usr --docdir=/usr/share/doc/${pkgname} -O"
        , "runhaskell Setup build"
        ]
    , arch_package =
        [ "cd ${srcdir}/" ++ hkgname ++ "-${pkgver}"
        , "runhaskell Setup copy --destdir=${pkgdir}"]
    , arch_install = Nothing
    }

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

findCLibs :: PackageDescription -> SystemProvides -> [String]
findCLibs (PackageDescription { library = lib, executables = exe }) sysContext =
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

    canonicalise k = case M.lookup k (translationTable sysContext) of
        Nothing -> trace ("WARNING: this library depends on a C library we do not know the pacman name for (" ++ map toLower k ++ ") . Check the C library names in the generated PKGBUILD File") $ map toLower k
        Just s  -> s

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

