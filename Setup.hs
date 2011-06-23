module Main (main) where

import Distribution.System ( buildOS, OS(..) )
import Distribution.Simple.InstallDirs ( libdir )
import Distribution.Simple.Setup ( ConfigFlags, fromFlag, configVerbosity
                                 , InstallFlags, defaultCopyFlags
                                 , installDistPref, CleanFlags
                                 , toFlag, CopyDest(NoCopyDest)
                                 , installVerbosity, cleanVerbosity
                                 , CopyFlags(..)
                                 , configProgramArgs )
import Distribution.Simple.Utils ( rawSystemExit, installOrdinaryFile )
import Distribution.Verbosity ( verbose, Verbosity )
import Distribution.Simple ( defaultMainWithHooks
                           , simpleUserHooks
                           , buildHook, Args, confHook
                           , UserHooks(preConf, preClean,
                                       postCopy, postInst) )
import Distribution.Simple.LocalBuildInfo ( absoluteInstallDirs
                                          , LocalBuildInfo(..) )
import Distribution.PackageDescription ( emptyBuildInfo
                                       , updatePackageDescription
                                       , HookedBuildInfo
                                       , BuildInfo(..)
                                       , PackageDescription )
import System.FilePath ( (</>) )

dynamicLibDir :: FilePath
dynamicLibDir = "build"

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { preConf   = \a f -> makeGlfw a f >> preConf simpleUserHooks a f
  , confHook  = \a cfs -> do
      let pkgForce = [("ghc-pkg",["--force"]) | buildOS == OSX ]
          cfs' = cfs { configProgramArgs = configProgramArgs cfs ++ pkgForce }
      confHook simpleUserHooks a cfs'
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
           "make" : (if verbosity >= verbose then [] else ["--quiet"])
      make
    _ -> return ()

makeClean :: Args -> CleanFlags -> IO ()
makeClean _ flags = do
  let verbosity = fromFlag $ cleanVerbosity flags
  case buildOS of
    OSX -> do
      let make = rawSystemExit verbosity "env" $
           ["make"] ++ (if verbosity >= verbose then [] else ["--quiet"])
            ++ ["clean"]
      make
    _ -> return ()

glfwPkgDesc :: PackageDescription -> PackageDescription
glfwPkgDesc pkgDesc =
  case buildOS of
    OSX -> updatePackageDescription libDirGlfw pkgDesc
    _   -> pkgDesc

libDirGlfw :: HookedBuildInfo
libDirGlfw = (Just buildinfo, [])
  where buildinfo = emptyBuildInfo
                     { extraLibDirs = [ dynamicLibDir ] }

postInstGlfw :: Args -> InstallFlags -> PackageDescription
             -> LocalBuildInfo -> IO ()
postInstGlfw _ flags pkgDesc lbi =
  case buildOS of
    OSX -> do
      let copyflags = defaultCopyFlags
                    { copyDistPref  = installDistPref flags
                    , copyDest      = toFlag NoCopyDest
                    , copyVerbosity = installVerbosity flags
                    }
      postCopyGlfw undefined copyflags pkgDesc lbi
    _ -> return ()

postCopyGlfw :: Args -> CopyFlags -> PackageDescription
             -> LocalBuildInfo -> IO ()
postCopyGlfw _ flags pkgDesc lbi =
  case buildOS of
    OSX -> do
      let installDirs = absoluteInstallDirs pkgDesc lbi
            . fromFlag . copyDest $ flags
          libPref = libdir installDirs
          verbosity = fromFlag $ copyVerbosity flags
          copyDynamic dest f = installOrdinaryFile verbosity (dynamicLibDir</>f) (dest</>f)
      maybe (return ()) (copyDynamic libPref) (Just dynamicLibName)
      install_name_tool verbosity (libPref</>dynamicLibName)
    _ -> return ()

install_name_tool :: Verbosity -> FilePath -> IO ()
install_name_tool v fp = rawSystemExit v "env" $
  ["install_name_tool"] ++ ["-id"] ++ [fp] ++ [fp]

dynamicLibName :: FilePath
dynamicLibName = "libglfw.dylib"
