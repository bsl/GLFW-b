-- base
import Control.Concurrent (threadDelay)
import Data.Char          (isAscii)
import Data.List          (intercalate, isPrefixOf)

-- HUnit
import Test.HUnit ((@?=), assertBool, assertFailure)

-- test-framework
import Test.Framework (Test, defaultMain, testGroup)

-- test-framework-hunit
import Test.Framework.Providers.HUnit (testCase)

-- GLFW-b
import qualified Graphics.UI.GLFW as GLFW

--------------------------------------------------------------------------------

main :: IO ()
main = do
    GLFW.setErrorCallback $ Just $ \e s ->
        putStrLn $ unwords ["###", show e, show s]

    True <- GLFW.init

    Just mon <- GLFW.getPrimaryMonitor

    GLFW.windowHint $ GLFW.WindowHint'Visible False
    mwin@(Just win) <- GLFW.createWindow 100 100 "GLFW-b test" Nothing Nothing
    GLFW.makeContextCurrent mwin

    defaultMain $ tests mon win

    -- TODO because of how defaultMain works, this code is not reached
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

between :: Ord a => a -> (a,a) -> Bool
between n (l,h) = n >= l && n <= h

videoModeLooksValid :: GLFW.VideoMode -> Bool
videoModeLooksValid vm = and
    [ GLFW.videoModeWidth       vm `between` (0,4000)
    , GLFW.videoModeHeight      vm `between` (0,2000)
    , GLFW.videoModeRedBits     vm `between` (0,  32)
    , GLFW.videoModeGreenBits   vm `between` (0,  32)
    , GLFW.videoModeBlueBits    vm `between` (0,  32)
    , GLFW.videoModeRefreshRate vm `between` (0, 120)
    ]

--------------------------------------------------------------------------------

tests :: GLFW.Monitor -> GLFW.Window -> [Test]
tests mon win =
    [ testGroup "Initialization and version information"
      [ testCase "getVersion"       test_getVersion
      , testCase "getVersionString" test_getVersionString
      ]
    , testGroup "Monitor handling"
      [ testCase "getMonitors"              test_getMonitors
      , testCase "getPrimaryMonitor"        test_getPrimaryMonitor
      , testCase "getMonitorPos"          $ test_getMonitorPos mon
      , testCase "getMonitorPhysicalSize" $ test_getMonitorPhysicalSize mon
      , testCase "getMonitorName"         $ test_getMonitorName mon
      , testCase "getVideoModes"          $ test_getVideoModes mon
      , testCase "getVideoMode"           $ test_getVideoMode mon
      , testCase "getGamma"               $ test_getGammaRamp mon
      -- , testCase "setGamma"               $ test_setGamma mon
      -- , testCase "gamma ramp"             $ test_gamma_ramp mon
      ]
    , testGroup "Window handling"
      [ testCase "defaultWindowHints"             test_defaultWindowHints
      , testCase "window close flag"            $ test_window_close_flag win
      , testCase "setWindowTitle"               $ test_setWindowTitle win
      , testCase "window pos"                   $ test_window_pos win
      , testCase "window size"                  $ test_window_size win
      , testCase "getFramebufferSize"           $ test_getFramebufferSize win
      , testCase "iconification"                $ test_iconification win
      -- , testCase "show/hide"                    $ test_show_hide win
      , testCase "getWindowMonitor"             $ test_getWindowMonitor win mon
      , testCase "cursor pos"                   $ test_cursor_pos win
      , testCase "getWindowFocused"             $ test_getWindowFocused win
      , testCase "getWindowResizable"           $ test_getWindowResizable win
      , testCase "getWindowDecorated"           $ test_getWindowDecorated win
      , testCase "getWindowClientAPI"           $ test_getWindowClientAPI win
      , testCase "window context version"       $ test_window_context_version win
      , testCase "getWindowContextRobustness"   $ test_getWindowContextRobustness win
      , testCase "getWindowOpenGLForwardCompat" $ test_getWindowOpenGLForwardCompat win
      , testCase "getWindowOpenGLDebugContext"  $ test_getWindowOpenGLDebugContext win
      , testCase "getWindowOpenGLProfile"       $ test_getWindowOpenGLProfile win
      , testCase "pollEvents"                     test_pollEvents
      , testCase "waitEvents"                     test_waitEvents
      ]
    , testGroup "Input handling"
      [ testCase "cursor input mode"               $ test_cursor_input_mode win
      , testCase "sticky keys input mode"          $ test_sticky_keys_input_mode win
      , testCase "sticky mouse buttons input mode" $ test_sticky_mouse_buttons_input_mode win
      , testCase "joystickPresent"                   test_joystickPresent
      , testCase "getJoystickAxes"                   test_getJoystickAxes
      , testCase "getJoystickButtons"                test_getJoystickButtons
      , testCase "getJoystickName"                   test_getJoystickName
      ]
    , testGroup "Time"
      [ testCase "getTime" test_getTime
      , testCase "setTime" test_setTime
      ]
    , testGroup "Context"
      [ testCase "getCurrentContext"  $ test_getCurrentContext win
      , testCase "swapBuffers"        $ test_swapBuffers win
      , testCase "swapInterval"         test_swapInterval
      , testCase "extensionSupported"   test_extensionSupported
      ]
    , testGroup "Clipboard"
      [ testCase "clipboard" $ test_clipboard win
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
    mvs <- GLFW.getVersionString
    case mvs of
      Just vs -> assertBool "" $ v `isPrefixOf` vs
      Nothing -> assertFailure ""
  where
    v = intercalate "." $ map show [versionMajor, versionMinor, versionRevision]

test_getMonitors :: IO ()
test_getMonitors = do
    r <- GLFW.getMonitors
    case r of
      Just ms -> assertBool "" $ (not . null) ms
      Nothing -> assertFailure ""

test_getPrimaryMonitor :: IO ()
test_getPrimaryMonitor = do
    r <- GLFW.getPrimaryMonitor
    case r of
      Just _  -> return ()
      Nothing -> assertFailure ""

test_getMonitorPos :: GLFW.Monitor -> IO ()
test_getMonitorPos mon = do
    (x, y) <- GLFW.getMonitorPos mon
    assertBool "" $ x >= 0
    assertBool "" $ y >= 0

test_getMonitorPhysicalSize :: GLFW.Monitor -> IO ()
test_getMonitorPhysicalSize mon = do
    (w, h) <- GLFW.getMonitorPhysicalSize mon
    assertBool "" $ w `between` (0, 1000)
    assertBool "" $ h `between` (0,  500)

test_getMonitorName :: GLFW.Monitor -> IO ()
test_getMonitorName mon = do
    mname <- GLFW.getMonitorName mon
    case mname of
      Nothing   -> assertFailure ""
      Just name -> do
          assertBool "" $ length name `between` (0, 20)
          assertBool "" $ all isAscii name

test_getVideoModes :: GLFW.Monitor -> IO ()
test_getVideoModes mon = do
    mvms <- GLFW.getVideoModes mon
    case mvms of
      Nothing  -> assertFailure ""
      Just vms -> assertBool "" $ all videoModeLooksValid vms

test_getVideoMode :: GLFW.Monitor -> IO ()
test_getVideoMode mon = do
    mvm <- GLFW.getVideoMode mon
    case mvm of
      Just vm -> assertBool "" $ videoModeLooksValid vm
      Nothing -> assertFailure ""

test_getGammaRamp :: GLFW.Monitor -> IO ()
test_getGammaRamp mon = do
    mgr <- GLFW.getGammaRamp mon
    case mgr of
      Nothing -> assertFailure ""
      Just gr -> assertBool "" $
        let rsl = length $ GLFW.gammaRampRed   gr
            gsl = length $ GLFW.gammaRampGreen gr
            bsl = length $ GLFW.gammaRampBlue  gr
        in rsl > 0 && rsl == gsl && gsl == bsl

--------------------------------------------------------------------------------

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
    GLFW.setWindowTitle win "some new title"

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

-- test_show_hide :: GLFW.Window -> IO ()
-- test_show_hide win = do
--     v0 <- GLFW.getWindowVisible win
--     v0 @?= True

--     GLFW.hideWindow win
--     giveItTime
--     v1 <- GLFW.getWindowVisible win
--     v1 @?= False

--     GLFW.showWindow win
--     giveItTime
--     v2 <- GLFW.getWindowVisible win
--     v2 @?= True

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
    assertBool "" $ all (`between` (0, 20)) [v0, v1, v2]

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

--------------------------------------------------------------------------------

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
    assertBool "" $ not r

test_getJoystickAxes :: IO ()
test_getJoystickAxes =
    mapM_ GLFW.getJoystickAxes joysticks

test_getJoystickButtons :: IO ()
test_getJoystickButtons =
    mapM_ GLFW.getJoystickButtons joysticks

test_getJoystickName :: IO ()
test_getJoystickName =
    mapM_ GLFW.getJoystickName joysticks

--------------------------------------------------------------------------------

test_getTime :: IO ()
test_getTime = do
    mt <- GLFW.getTime
    case mt of
      Nothing -> assertFailure ""
      Just t  -> assertBool "" $ t `between` (0, 10)

test_setTime :: IO ()
test_setTime = do
    let t = 37
    GLFW.setTime t
    mt <- GLFW.getTime
    case mt of
      Just t' -> assertBool "" $ t' `between` (t, t+10)
      Nothing -> assertFailure ""

--------------------------------------------------------------------------------

test_getCurrentContext :: GLFW.Window -> IO ()
test_getCurrentContext win = do
    mwin <- GLFW.getCurrentContext
    case mwin of
      Nothing   -> assertFailure ""
      Just win' -> win' @?= win

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

--------------------------------------------------------------------------------

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

--------------------------------------------------------------------------------

{-# ANN module "HLint: ignore Use camelCase" #-}
