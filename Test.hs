import Control.Concurrent (threadDelay)
import Control.Monad      (mapM, mapM_)
import Data.List          (intercalate, isPrefixOf)
import Data.Maybe         (isJust)

import           Test.HUnit ((@?=))
import qualified Test.Framework                 as TF
import qualified Test.Framework.Providers.HUnit as TF
import qualified Test.HUnit                     as HU

import qualified Graphics.UI.GLFW as GLFW

--------------------------------------------------------------------------------

main :: IO ()
main = do
    GLFW.setErrorCallback $ Just $ \e s ->
        putStrLn $ unwords ["###", show e, show s]

    True <- GLFW.init
    (Just mon) <- GLFW.getPrimaryMonitor
    GLFW.windowHint $ GLFW.WindowHint'Visible False
    mwin@(Just win) <- GLFW.createWindow 100 100 "GLFW-b test" Nothing Nothing
    GLFW.makeContextCurrent mwin

    TF.defaultMain $ tests mon win

    GLFW.destroyWindow win
    GLFW.terminate

--------------------------------------------------------------------------------

versionMajor, versionMinor, versionRevision :: Int
versionMajor    = 3
versionMinor    = 0
versionRevision = 1

version :: GLFW.Version
version = GLFW.Version versionMajor versionMinor versionRevision

giveItTime :: IO ()
giveItTime = threadDelay 250000

joysticks :: [GLFW.Joystick]
joysticks =
    [ GLFW.Joystick'1
    , GLFW.Joystick'2
    , GLFW.Joystick'3
    , GLFW.Joystick'4
    , GLFW.Joystick'5
    , GLFW.Joystick'6
    , GLFW.Joystick'7
    , GLFW.Joystick'8
    , GLFW.Joystick'9
    , GLFW.Joystick'10
    , GLFW.Joystick'11
    , GLFW.Joystick'12
    , GLFW.Joystick'13
    , GLFW.Joystick'14
    , GLFW.Joystick'15
    , GLFW.Joystick'16
    ]

--------------------------------------------------------------------------------

tests :: GLFW.Monitor -> GLFW.Window -> [TF.Test]
tests mon win =
    [ TF.testGroup "Initialization and version information"
      [ TF.testCase "getVersion"       test_getVersion
      , TF.testCase "getVersionString" test_getVersionString
      ]

    , TF.testGroup "Monitor handling"
      [ TF.testCase "getMonitors"              test_getMonitors
      , TF.testCase "getPrimaryMonitor"        test_getPrimaryMonitor
      , TF.testCase "getMonitorPos"          $ test_getMonitorPos mon
      , TF.testCase "getMonitorPhysicalSize" $ test_getMonitorPhysicalSize mon
      , TF.testCase "getMonitorName"         $ test_getMonitorName mon
      , TF.testCase "getVideoModes"          $ test_getVideoModes mon
      , TF.testCase "getVideoMode"           $ test_getVideoMode mon
      -- , TF.testCase "setGamma"               $ test_setGamma mon
      -- , TF.testCase "gamma ramp"             $ test_gamma_ramp mon
      ]

    , TF.testGroup "Window handling"
      [ TF.testCase "defaultWindowHints"             test_defaultWindowHints
      , TF.testCase "window close flag"            $ test_window_close_flag win
      , TF.testCase "setWindowTitle"               $ test_setWindowTitle win
      , TF.testCase "window pos"                   $ test_window_pos win
      , TF.testCase "window size"                  $ test_window_size win
      , TF.testCase "getFramebufferSize"           $ test_getFramebufferSize win
      , TF.testCase "iconification"                $ test_iconification win
      , TF.testCase "show/hide"                    $ test_show_hide win
      , TF.testCase "getWindowMonitor"             $ test_getWindowMonitor win mon
      , TF.testCase "cursor pos"                   $ test_cursor_pos win
      , TF.testCase "getWindowFocused"             $ test_getWindowFocused win
      , TF.testCase "getWindowResizable"           $ test_getWindowResizable win
      , TF.testCase "getWindowDecorated"           $ test_getWindowDecorated win
      , TF.testCase "getWindowClientAPI"           $ test_getWindowClientAPI win
      , TF.testCase "window context version"       $ test_window_context_version win
      , TF.testCase "getWindowContextRobustness"   $ test_getWindowContextRobustness win
      , TF.testCase "getWindowOpenGLForwardCompat" $ test_getWindowOpenGLForwardCompat win
      , TF.testCase "getWindowOpenGLDebugContext"  $ test_getWindowOpenGLDebugContext win
      , TF.testCase "getWindowOpenGLProfile"       $ test_getWindowOpenGLProfile win
      , TF.testCase "pollEvents"                     test_pollEvents
      , TF.testCase "waitEvents"                     test_waitEvents
      ]

    , TF.testGroup "Input handling"
      [ TF.testCase "cursor input mode"               $ test_cursor_input_mode win
      , TF.testCase "sticky keys input mode"          $ test_sticky_keys_input_mode win
      , TF.testCase "sticky mouse buttons input mode" $ test_sticky_mouse_buttons_input_mode win
        -- TODO callbacks
      , TF.testCase "joystickPresent"                   test_joystickPresent
      , TF.testCase "getJoystickAxes"                   test_getJoystickAxes
      , TF.testCase "getJoystickButtons"                test_getJoystickButtons
      , TF.testCase "getJoystickName"                   test_getJoystickName
      ]

    , TF.testGroup "Time"
      [ TF.testCase "getTime" test_getTime
      , TF.testCase "setTime" test_setTime
      ]

    , TF.testGroup "Context"
      [ TF.testCase "getCurrentContext"  $ test_getCurrentContext win
      , TF.testCase "swapBuffers"        $ test_swapBuffers win
      , TF.testCase "swapInterval"         test_swapInterval
      , TF.testCase "extensionSupported"   test_extensionSupported
      ]

    , TF.testGroup "Clipboard"
      [ TF.testCase "clipboard" $ test_clipboard win
      ]
    ]

--------------------------------------------------------------------------------

test_getVersion :: IO ()
test_getVersion = do
    v <- GLFW.getVersion
    v                      @?= version
    GLFW.versionMajor    v @?= versionMajor
    GLFW.versionMinor    v @?= versionMinor
    GLFW.versionRevision v @?= versionRevision

test_getVersionString :: IO ()
test_getVersionString = do
    vs <- GLFW.getVersionString
    HU.assertBool "" $ v `isPrefixOf` vs
  where
    v = intercalate "." $ map show [versionMajor, versionMinor, versionRevision]

test_getMonitors :: IO ()
test_getMonitors = do
    r <- GLFW.getMonitors
    case r of
      (Just ms) -> HU.assertBool "" $ (not . null) ms
      Nothing   -> HU.assertFailure ""

test_getPrimaryMonitor :: IO ()
test_getPrimaryMonitor = do
    r <- GLFW.getPrimaryMonitor
    case r of
      (Just _) -> return ()
      Nothing  -> HU.assertFailure ""

test_getMonitorPos :: GLFW.Monitor -> IO ()
test_getMonitorPos mon = do
    (x, y) <- GLFW.getMonitorPos mon
    HU.assertBool "" $ x >= 0
    HU.assertBool "" $ y >= 0

test_getMonitorPhysicalSize :: GLFW.Monitor -> IO ()
test_getMonitorPhysicalSize mon = do
    (w, h) <- GLFW.getMonitorPhysicalSize mon
    HU.assertBool "" $ w >= 0
    HU.assertBool "" $ h >= 0

test_getMonitorName :: GLFW.Monitor -> IO ()
test_getMonitorName mon = do
    r <- GLFW.getMonitorName mon
    HU.assertBool "" $ isJust r

test_getVideoModes :: GLFW.Monitor -> IO ()
test_getVideoModes mon = do
    r <- GLFW.getVideoModes mon
    case r of
      (Just _) -> return ()  -- TODO do more strict checking here
      Nothing  -> HU.assertFailure ""

test_getVideoMode :: GLFW.Monitor -> IO ()
test_getVideoMode mon = do
    r <- GLFW.getVideoMode mon
    case r of
      (Just _) -> return ()  -- TODO do more strict checking here
      Nothing  -> HU.assertFailure ""

-- test_setGamma :: GLFW.Monitor -> IO ()
-- test_setGamma mon = do
--     _ <- GLFW.setGamma mon 37
--     return ()

test_getGammaRamp :: GLFW.Monitor -> IO ()
test_getGammaRamp mon = do
    m <- GLFW.getGammaRamp mon
    case m of
      (Just gr) -> HU.assertBool "" $
          let rl = length $ GLFW.gammaRampRed   gr
              gl = length $ GLFW.gammaRampGreen gr
              bl = length $ GLFW.gammaRampBlue  gr
          in rl > 0 && rl == gl && gl == bl
      Nothing -> HU.assertFailure ""

-- test_setGammaRamp :: GLFW.Monitor -> IO ()
-- test_setGammaRamp mon =

test_defaultWindowHints :: IO ()
test_defaultWindowHints =
    GLFW.defaultWindowHints

test_window_close_flag :: GLFW.Window -> IO ()
test_window_close_flag win = do
    r0 <- GLFW.windowShouldClose win
    r0 @?= False

    GLFW.setWindowShouldClose win True
    r1 <- GLFW.windowShouldClose win
    r1 @?= True

    GLFW.setWindowShouldClose win False
    r2 <- GLFW.windowShouldClose win
    r2 @?= False

test_setWindowTitle :: GLFW.Window -> IO ()
test_setWindowTitle win =
    GLFW.setWindowTitle win "x"

-- This is a little strange. Depending on your window manager, etc, after
-- setting the window position to (x,y), the actual new window position might
-- be (x+5,y+5) due to borders. So we just check for consistency.
test_window_pos :: GLFW.Window -> IO ()
test_window_pos win = do
    let x = 17
        y = 37
        xoff = 53
        yoff = 149
    (x0, y0, dx0, dy0) <- setGet  x        y
    (x1, y1, dx1, dy1) <- setGet (x+xoff) (y+yoff)
    dx0 @?= dx1
    dy0 @?= dy1
    x1 - x0 @?= xoff
    y1 - y0 @?= yoff
  where
    setGet x0 y0 = do
        GLFW.setWindowPos win x0 y0
        giveItTime
        (x1, y1) <- GLFW.getWindowPos win
        let (dx, dy) = (x0-x1, y0-y1)
        return (x1, y1, dx, dy)

test_window_size :: GLFW.Window -> IO ()
test_window_size win = do
    let w = 17
        h = 37
    GLFW.setWindowSize win w h
    giveItTime
    (w', h') <- GLFW.getWindowSize win
    w' @?= w
    h' @?= h

test_getFramebufferSize :: GLFW.Window -> IO ()
test_getFramebufferSize win = do
    (w, h) <- GLFW.getWindowSize win
    (fw, fh) <- GLFW.getFramebufferSize win
    fw @?= w
    fh @?= h

test_iconification :: GLFW.Window -> IO ()
test_iconification win = do
    is0 <- GLFW.getWindowIconified win
    is0 @?= GLFW.IconifyState'NotIconified

    GLFW.iconifyWindow win
    giveItTime
    is1 <- GLFW.getWindowIconified win
    is1 @?= GLFW.IconifyState'Iconified

    GLFW.restoreWindow win

test_show_hide :: GLFW.Window -> IO ()
test_show_hide win = do
    v0 <- GLFW.getWindowVisible win
    v0 @?= True

    -- GLFW.hideWindow win
    -- giveItTime
    -- v1 <- GLFW.getWindowVisible win
    -- v1 @?= False

    -- GLFW.showWindow win
    -- giveItTime
    -- v2 <- GLFW.getWindowVisible win
    -- v2 @?= True

test_getWindowMonitor :: GLFW.Window -> GLFW.Monitor -> IO ()
test_getWindowMonitor win _ = do
    m <- GLFW.getWindowMonitor win
    m @?= Nothing

test_cursor_pos :: GLFW.Window -> IO ()
test_cursor_pos win = do
    (w, h) <- GLFW.getWindowSize win
    let cx = fromIntegral w / 2
        cy = fromIntegral h / 2
    GLFW.setCursorPos win cx cy
    giveItTime
    (cx', cy') <- GLFW.getCursorPos win
    cx' @?= cx
    cy' @?= cy

test_getWindowFocused :: GLFW.Window -> IO ()
test_getWindowFocused win = do
    fs <- GLFW.getWindowFocused win
    fs @?= GLFW.FocusState'Defocused

test_getWindowResizable :: GLFW.Window -> IO ()
test_getWindowResizable win = do
    b <- GLFW.getWindowResizable win
    b @?= True

test_getWindowDecorated :: GLFW.Window -> IO ()
test_getWindowDecorated win = do
    b <- GLFW.getWindowDecorated win
    b @?= True

test_getWindowClientAPI :: GLFW.Window -> IO ()
test_getWindowClientAPI win = do
    a <- GLFW.getWindowClientAPI win
    a @?= GLFW.ClientAPI'OpenGL

test_window_context_version :: GLFW.Window -> IO ()
test_window_context_version win = do
    v0 <- GLFW.getWindowContextVersionMajor    win
    v1 <- GLFW.getWindowContextVersionMinor    win
    v2 <- GLFW.getWindowContextVersionRevision win
    HU.assertBool "" $
      all (\n -> n >= 0 && n < 10)
          [v0, v1, v2]

test_getWindowContextRobustness :: GLFW.Window -> IO ()
test_getWindowContextRobustness win = do
    _ <- GLFW.getWindowContextRobustness win
    return ()

test_getWindowOpenGLForwardCompat :: GLFW.Window -> IO ()
test_getWindowOpenGLForwardCompat win = do
    _ <- GLFW.getWindowOpenGLForwardCompat win
    return ()

test_getWindowOpenGLDebugContext :: GLFW.Window -> IO ()
test_getWindowOpenGLDebugContext win = do
    _ <- GLFW.getWindowOpenGLDebugContext win
    return ()

test_getWindowOpenGLProfile :: GLFW.Window -> IO ()
test_getWindowOpenGLProfile win = do
    _ <- GLFW.getWindowOpenGLProfile win
    return ()

test_pollEvents :: IO ()
test_pollEvents =
    GLFW.pollEvents

test_waitEvents :: IO ()
test_waitEvents =
    GLFW.waitEvents

test_cursor_input_mode :: GLFW.Window -> IO ()
test_cursor_input_mode win = do
    modes' <- mapM setGet modes
    modes' @?= modes
  where
    modes =
      [ GLFW.CursorInputMode'Disabled
      , GLFW.CursorInputMode'Hidden
      , GLFW.CursorInputMode'Normal
      ]
    setGet m = do
        GLFW.setCursorInputMode win m
        GLFW.getCursorInputMode win

test_sticky_keys_input_mode :: GLFW.Window -> IO ()
test_sticky_keys_input_mode win = do
    modes' <- mapM setGet modes
    modes' @?= modes
  where
    modes =
      [ GLFW.StickyKeysInputMode'Enabled
      , GLFW.StickyKeysInputMode'Disabled
      ]
    setGet m = do
        GLFW.setStickyKeysInputMode win m
        GLFW.getStickyKeysInputMode win

test_sticky_mouse_buttons_input_mode :: GLFW.Window -> IO ()
test_sticky_mouse_buttons_input_mode win = do
    modes' <- mapM setGet modes
    modes' @?= modes
  where
    modes =
      [ GLFW.StickyMouseButtonsInputMode'Enabled
      , GLFW.StickyMouseButtonsInputMode'Disabled
      ]
    setGet m = do
        GLFW.setStickyMouseButtonsInputMode win m
        GLFW.getStickyMouseButtonsInputMode win

test_joystickPresent :: IO ()
test_joystickPresent = do
    _ <- GLFW.joystickPresent GLFW.Joystick'1
    r <- GLFW.joystickPresent GLFW.Joystick'16
    HU.assertBool "error" $ not r

test_getJoystickAxes :: IO ()
test_getJoystickAxes =
    mapM_ GLFW.getJoystickAxes joysticks

test_getJoystickName :: IO ()
test_getJoystickName =
    mapM_ GLFW.getJoystickName joysticks

test_getJoystickButtons :: IO ()
test_getJoystickButtons =
    mapM_ GLFW.getJoystickButtons joysticks

test_getTime :: IO ()
test_getTime = do
    r <- GLFW.getTime
    case r of
      (Just _) -> return ()  -- TODO do more strict checking here
      Nothing  -> HU.assertFailure ""

test_setTime :: IO ()
test_setTime = do
    let t = 37
    GLFW.setTime t
    r <- GLFW.getTime
    case r of
      (Just t') -> HU.assertBool "" (t' >= t)
      Nothing   -> HU.assertFailure ""

test_getCurrentContext :: GLFW.Window -> IO ()
test_getCurrentContext win = do
    mwin <- GLFW.getCurrentContext
    case mwin of
      (Just win') -> win' @?= win
      Nothing     -> HU.assertFailure ""

test_swapBuffers :: GLFW.Window -> IO ()
test_swapBuffers =
    GLFW.swapBuffers

test_swapInterval :: IO ()
test_swapInterval =
    GLFW.swapInterval 1

test_extensionSupported :: IO ()
test_extensionSupported = do
    b0 <- GLFW.extensionSupported "GL_ARB_multisample"
    b0 @?= True
    b1 <- GLFW.extensionSupported "bogus"
    b1 @?= False

test_clipboard :: GLFW.Window -> IO ()
test_clipboard win = do
    rs <- mapM setGet ss
    rs @?= map Just ss
  where
    ss =
      [ "abc 123 ???"
      , "xyz 456 !!!"
      ]
    setGet s = do
        GLFW.setClipboardString win s
        GLFW.getClipboardString win

{-# ANN module "HLint: ignore Use camelCase" #-}
