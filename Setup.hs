module Main (main) where

import Distribution.System ( buildOS, OS(..) )
import Distribution.Simple.InstallDirs ( libdir, bindir )
import Distribution.Simple.Setup ( ConfigFlags, fromFlag, configVerbosity
                                 , InstallFlags, defaultCopyFlags
                                 , installDistPref, CleanFlags
                                 , toFlag, CopyDest(NoCopyDest)
                                 , installVerbosity, cleanVerbosity
                                 , CopyFlags(..) )
import Distribution.Simple.Utils ( rawSystemExit, installOrdinaryFile
                                 , installExecutableFile, copyFileVerbose
                                 , createDirectoryIfMissingVerbose )
import Distribution.Verbosity ( verbose )
import Distribution.Simple ( defaultMainWithHooks
                           , simpleUserHooks
                           , buildHook, Args
                           , UserHooks(preConf, preClean,
                                       postCopy, postInst) )
import Distribution.Simple.LocalBuildInfo ( absoluteInstallDirs
                                          , LocalBuildInfo )
import Distribution.PackageDescription ( emptyBuildInfo
                                       , updatePackageDescription
                                       , HookedBuildInfo(..)
                                       , BuildInfo(..)
                                       , PackageDescription )
import System.Cmd ( system )
import System.FilePath ( (</>) )

extraLibDir = "dist"</>"build"

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preConf   = \a f -> makeGlfw a f >> preConf simpleUserHooks a f
  , preClean  = \a f -> makeClean a f >> preClean simpleUserHooks a f
  , buildHook = \pkgDesc lbi h f ->
      buildHook simpleUserHooks (glfwPkgDesc pkgDesc) lbi h f
  , postCopy  = postCopyGlfw
  , postInst  = postInstGlfw
  }

makeGlfw :: Args -> ConfigFlags -> IO ()
makeGlfw _ flags = do
  let verbosity = fromFlag $ configVerbosity flags
  case buildOS of
    OSX -> do
      let make = rawSystemExit verbosity "env" $
           ["make"] ++ (if verbosity >= verbose then [] else ["--quiet"])
      make

makeClean :: Args -> CleanFlags -> IO ()
makeClean _ flags = do
  let verbosity = fromFlag $ cleanVerbosity flags
  case buildOS of
    OSX -> do
      let make = rawSystemExit verbosity "env" $
           ["make"] ++ (if verbosity >= verbose then [] else ["--quiet"])
            ++ ["clean"]
      make

glfwPkgDesc pkgDesc = updatePackageDescription libDirGlfw pkgDesc

libDirGlfw :: HookedBuildInfo
libDirGlfw = (Just buildinfo, [])
  where buildinfo = emptyBuildInfo { extraLibDirs = [ extraLibDir ] }

postInstGlfw :: Args -> InstallFlags -> PackageDescription
             -> LocalBuildInfo -> IO ()
postInstGlfw _ flags pkgDesc lbi = do
  let copyflags = defaultCopyFlags
                    { copyDistPref  = installDistPref flags
                    , copyDest      = toFlag NoCopyDest
                    , copyVerbosity = installVerbosity flags
                    }
  postCopyGlfw undefined copyflags pkgDesc lbi

postCopyGlfw :: Args -> CopyFlags -> PackageDescription
             -> LocalBuildInfo -> IO ()
postCopyGlfw _ flags pkgDesc lbi = do
  let installDirs = absoluteInstallDirs pkgDesc lbi
        . fromFlag . copyDest $ flags
      libPref = libdir installDirs
      binPref = bindir installDirs
      verbosity = fromFlag $ copyVerbosity flags
      outDir = libPref
      copy dest f = installOrdinaryFile verbosity (extraLibDir</>f) (dest</>f)
  maybe (return ()) (copy libPref) (Just libName)
  maybe (return ()) (copy libPref) (Just sharedName)

libName = "libglfw.a"
sharedName = "libglfw.dylib"
