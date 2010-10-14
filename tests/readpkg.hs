import Distribution.ArchLinux.PkgBuild

import System.IO
import Text.PrettyPrint

--
-- | This test parses the file "PKGBUILD" and prints back the data.
--

main = do
  s <- readFile ("PKGBUILD")
  let t = decodePackage s
  case t of
    Left _ -> putStrLn "error"
    Right pkg -> putStr $ render $ pkg2doc "mail@example.org" pkg
