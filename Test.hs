import qualified Test.Framework                 as TF
import qualified Test.Framework.Providers.HUnit as TF
import qualified Test.HUnit                     as HU

import qualified Graphics.UI.GLFW as GLFW

import Data.List (intercalate, isPrefixOf)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

main :: IO ()
main = do
    GLFW.initialize
    TF.defaultMain tests
    GLFW.terminate

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

versionMajor, versionMinor, versionRevision :: Int
versionMajor    = 3
versionMinor    = 0
versionRevision = 0

glfwVersion :: GLFW.Version
glfwVersion = GLFW.Version versionMajor versionMinor versionRevision

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

tests :: [TF.Test]
tests =
    [ TF.testGroup "main"
        [ TF.testCase "getVersion"       test_getVersion
        , TF.testCase "getVersionString" test_getVersionString
        ]
    ]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

test_getVersion :: IO ()
test_getVersion = do
    v <- GLFW.getVersion
    HU.assertBool "(==)" $
        v == glfwVersion
    HU.assertBool "values" $
        GLFW.versionMajor    v == versionMajor    &&
        GLFW.versionMinor    v == versionMinor    &&
        GLFW.versionRevision v == versionRevision

test_getVersionString :: IO ()
test_getVersionString = do
    vs <- GLFW.getVersionString
    HU.assertBool "version" $
        let ver = intercalate "." $ map show [versionMajor, versionMinor, versionRevision]
        in ver `isPrefixOf` vs

{-# ANN module "HLint: ignore Use camelCase" #-}
