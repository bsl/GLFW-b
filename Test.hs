-- base
import Control.Concurrent (threadDelay)
import Control.Monad      (forM_)
import Data.Char          (isAscii)
import Data.Bits          (xor)
import Data.List          (intercalate, isPrefixOf)
import Data.Maybe         (isNothing)

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

    GLFW.initHint GLFW.InitHint'CocoaChdirResources False
    True <- GLFW.init

    Just mon <- GLFW.getPrimaryMonitor

    GLFW.windowHint $ GLFW.WindowHint'Visible False
    GLFW.windowHint $ GLFW.WindowHint'CocoaFrameName "GLFW-b test"
    mwin@(Just win) <- GLFW.createWindow 100 100 "GLFW-b test" Nothing Nothing
    GLFW.makeContextCurrent mwin

    -- Mostly check for compiling
    GLFW.setJoystickCallback $ Just $ \j c -> putStrLn $ concat
      [ show j, " changed state: ", show c ]

    defaultMain $ tests mon win

    -- TODO because of how defaultMain works, this code is not reached
    GLFW.destroyWindow win
    GLFW.terminate

--------------------------------------------------------------------------------

versionMajor, versionMinor, versionRevision :: Int
versionMajor = 3
versionMinor = 3
versionRevision = 9

giveItTime :: IO ()
giveItTime = threadDelay 500000

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
    , GLFW.videoModeHeight      vm `between` (0,3000)
    , GLFW.videoModeRedBits     vm `between` (0,  32)
    , GLFW.videoModeGreenBits   vm `between` (0,  32)
    , GLFW.videoModeBlueBits    vm `between` (0,  32)
    , GLFW.videoModeRefreshRate vm `between` (0, 120)
    ]

--------------------------------------------------------------------------------

glfwTest :: String -> IO () -> Test
glfwTest name test = testCase name $ do
  GLFW.clearError
  test
  err <- GLFW.getError
  case err of
    Nothing -> return ()
    Just e -> assertFailure $
              concat ["Test '", name, "' generated error: ", show e]

tests :: GLFW.Monitor -> GLFW.Window -> [Test]
tests mon win =
    [ testGroup "Initialization and version information"
      [ testCase "getVersion"       test_getVersion
      , testCase "getVersionString" test_getVersionString
      , testCase "getError"         test_getError
      ]
    , testGroup "Monitor handling"
      [ glfwTest "getMonitors"              test_getMonitors
      , glfwTest "getPrimaryMonitor"        test_getPrimaryMonitor
      , glfwTest "getMonitorPos"          $ test_getMonitorPos mon
      , glfwTest "getMonitorPhysicalSize" $ test_getMonitorPhysicalSize mon
      , glfwTest "getMonitorWorkarea"     $ test_getMonitorWorkarea mon
      , glfwTest "getMonitorName"         $ test_getMonitorName mon
      , glfwTest "getVideoModes"          $ test_getVideoModes mon
      , glfwTest "getVideoMode"           $ test_getVideoMode mon
      , glfwTest "getGamma"               $ test_getGammaRamp mon
      ]
    , testGroup "Window handling"
      [ glfwTest "defaultWindowHints"             test_defaultWindowHints

      -- Test window attributes
      , glfwTest "getWindowFocused"             $ test_getWindowFocused win
      , glfwTest "getWindowResizable"           $ test_getWindowResizable win
      , glfwTest "getWindowDecorated"           $ test_getWindowDecorated win
      , glfwTest "getWindowClientAPI"           $ test_getWindowClientAPI win
      , glfwTest "window context version"       $ test_window_context_version win
      , glfwTest "getWindowContextRobustness"   $ test_getWindowContextRobustness win
      , glfwTest "getWindowOpenGLForwardCompat" $ test_getWindowOpenGLForwardCompat win
      , glfwTest "getWindowOpenGLDebugContext"  $ test_getWindowOpenGLDebugContext win
      , glfwTest "getWindowOpenGLProfile"       $ test_getWindowOpenGLProfile win
      , glfwTest "window attribs"               $ test_windowAttribs win
      , glfwTest "window close flag"            $ test_window_close_flag win
      , glfwTest "setWindowOpacity"             $ test_window_opacity win
      , glfwTest "setWindowTitle"               $ test_setWindowTitle win
      , glfwTest "window pos"                   $ test_window_pos win
      , glfwTest "window size"                  $ test_window_size win
      , glfwTest "getWindowFrameSize"           $ test_getWindowFrameSize win
      , glfwTest "getFramebufferSize"           $ test_getFramebufferSize win
      , glfwTest "iconification"                $ test_iconification win
      -- , glfwTest "show/hide"                    $ test_show_hide win
      , glfwTest "getWindowMonitor"             $ test_getWindowMonitor win mon
      , glfwTest "setWindowed"                  $ test_setWindowed win
      , glfwTest "setWindowIcon"                $ test_setWindowIcon win
      , glfwTest "maximizeWindow"               $ test_maximizeWindow win
      , glfwTest "setWindowSizeLimits"          $ test_setWindowSizeLimits win
      , glfwTest "setWindowAspectRatio"         $ test_setWindowAspectRatio win
      , glfwTest "focusWindow"                  $ test_focusWindow win
      , glfwTest "cursor pos"                   $ test_cursor_pos win
      , glfwTest "pollEvents"                     test_pollEvents
      , glfwTest "waitEvents"                     test_waitEvents
      , glfwTest "waitEventsTimeout"              test_waitEventsTimeout
      ]
    , testGroup "Input handling"
      [ glfwTest "cursor input mode"               $ test_cursor_input_mode win
      , glfwTest "sticky keys input mode"          $ test_sticky_keys_input_mode win
      , glfwTest "sticky mouse buttons input mode" $ test_sticky_mouse_buttons_input_mode win
      , glfwTest "joystickPresent"                   test_joystickPresent
      , glfwTest "getJoystickAxes"                   test_getJoystickAxes
      , glfwTest "getJoystickButtons"                test_getJoystickButtons
      , glfwTest "getJoystickName"                   test_getJoystickName
      , glfwTest "getKeyName"                        test_getKeyName
      , glfwTest "getGamepadState"                   test_getGamepadState
      ]
    , testGroup "Time"
      [ glfwTest "getTime"              test_getTime
      , glfwTest "setTime"              test_setTime
      , glfwTest "getTimerValue"        test_getTimerValue
      , glfwTest "getTimerFrequency"    test_getTimerFrequency
      ]
    , testGroup "Context"
      [ glfwTest "getCurrentContext"  $ test_getCurrentContext win
      , glfwTest "swapBuffers"        $ test_swapBuffers win
      , glfwTest "swapInterval"         test_swapInterval
      , glfwTest "extensionSupported"   test_extensionSupported
      ]
    , testGroup "Clipboard"
      [ glfwTest "clipboard" $ test_clipboard win
      ]
    ]

--------------------------------------------------------------------------------

test_getVersion :: IO ()
test_getVersion = do
    v <- GLFW.getVersion
    GLFW.versionMajor v @?= versionMajor
    GLFW.versionMinor v @?= versionMinor
    GLFW.versionRevision v @?= versionRevision

test_getVersionString :: IO ()
test_getVersionString = do
    mvs <- GLFW.getVersionString
    case mvs of
      Just vs -> assertBool "" $ v `isPrefixOf` vs
      Nothing -> assertFailure ""
  where
    v = intercalate "." $ map show [versionMajor, versionMinor]

test_getError :: IO ()
test_getError = GLFW.getError >>= assertBool "Got GLFW error!" . isNothing

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

test_getMonitorWorkarea :: GLFW.Monitor -> IO ()
test_getMonitorWorkarea mon = do
    (x, y, w, h) <- GLFW.getMonitorWorkarea mon
    assertBool "workarea x nonnegative" $ x >= 0
    assertBool "workarea y nonnegative" $ y >= 0
    assertBool "workarea width positive" $ w > 0
    assertBool "workarea height positive" $ h > 0

test_getMonitorName :: GLFW.Monitor -> IO ()
test_getMonitorName mon = do
    mname <- GLFW.getMonitorName mon
    case mname of
      Nothing   -> assertFailure ""
      Just name -> assertBool "" $ all isAscii name

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

test_windowAttribs :: GLFW.Window -> IO ()
test_windowAttribs win = do
    oldRsz <- GLFW.getWindowAttrib win GLFW.WindowAttrib'Resizable

    GLFW.setWindowAttrib win GLFW.WindowAttrib'Resizable False
    GLFW.getWindowAttrib win GLFW.WindowAttrib'Resizable >>= (@?= False)

    GLFW.setWindowAttrib win GLFW.WindowAttrib'Resizable True
    rsz <- GLFW.getWindowAttrib win GLFW.WindowAttrib'Resizable
    rsz @?= True

    GLFW.setWindowAttrib win GLFW.WindowAttrib'Resizable oldRsz

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

test_window_opacity :: GLFW.Window -> IO ()
test_window_opacity win = do
    oldOpacity <- GLFW.getWindowOpacity win

    GLFW.setWindowOpacity win 0.27
    opacity <- GLFW.getWindowOpacity win
    assertBool "Opacity is close to what we set it to." $
      (abs (opacity - 0.27)) < 0.01

    GLFW.setWindowOpacity win oldOpacity

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
    let w = 170
        h = 370
    GLFW.setWindowSize win w h
    giveItTime
    (w', h') <- GLFW.getWindowSize win
    w' @?= w
    h' @?= h

test_getWindowFrameSize :: GLFW.Window -> IO ()
test_getWindowFrameSize win = do
    -- The frame size is pretty dependent on the window manager. We can only
    -- really expect that the window has a title bar, so the top frame won't be
    -- zero...
    (_, t, _, _) <- GLFW.getWindowFrameSize win
    assertBool "Window has no frame width up top!" $ t > 0

test_getFramebufferSize :: GLFW.Window -> IO ()
test_getFramebufferSize win = do
    (w, h) <- GLFW.getWindowSize win
    (fw, fh) <- GLFW.getFramebufferSize win
    -- Window size and framebuffer size are not always equal, an example are
    -- retina screens. But maybe it's safe to assume they'll always be scaled
    -- equally in both dimensions?
    let ww = ( fromIntegral fw / fromIntegral w ) :: Double
        hh = ( fromIntegral fh / fromIntegral h ) :: Double
    ww @?= hh
    assertBool "" $ fw /= 0
    assertBool "" $ fh /= 0

test_iconification :: GLFW.Window -> IO ()
test_iconification win = do
    GLFW.showWindow win
    is0 <- GLFW.getWindowIconified win
    is0 @?= False

    GLFW.iconifyWindow win
    giveItTime
    is1 <- GLFW.getWindowIconified win
    is1 @?= True

    GLFW.restoreWindow win
    GLFW.hideWindow win

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

test_setWindowed :: GLFW.Window -> IO ()
test_setWindowed win = GLFW.setWindowed win 100 100 0 0

test_setWindowIcon :: GLFW.Window -> IO ()
test_setWindowIcon win = let
  icon1 = GLFW.mkImage 32 32 $ \x y ->
    case even ((x `div` 8) `xor` (y `div` 8)) of
      True -> (255, 0, 0, 255)
      False -> (0, 255, 0, 255)

  icon2 = GLFW.mkImage 16 16 $ \x y ->
    case even ((x `div` 8) `xor` (y `div` 8)) of
      True -> (255, 255, 255, 255)
      False -> (0, 0, 0, 255)

  in GLFW.setWindowIcon win [icon1, icon2]

test_maximizeWindow :: GLFW.Window -> IO ()
test_maximizeWindow win = do
  GLFW.showWindow win
  startsMaximized <- GLFW.getWindowMaximized win
  startsMaximized @?= False

  GLFW.maximizeWindow win
  giveItTime

  isMaximized <- GLFW.getWindowMaximized win
  isMaximized @?= True
  GLFW.hideWindow win

test_setWindowSizeLimits :: GLFW.Window -> IO ()
test_setWindowSizeLimits win = do
    GLFW.setWindowSizeLimits win (Just 640) (Just 480) (Just 1024) (Just 768)

test_setWindowAspectRatio :: GLFW.Window -> IO ()
test_setWindowAspectRatio win = do
    GLFW.setWindowAspectRatio win Nothing

test_focusWindow :: GLFW.Window -> IO ()
test_focusWindow = GLFW.focusWindow

test_cursor_pos :: GLFW.Window -> IO ()
test_cursor_pos win = do
  GLFW.showWindow win
  (w, h) <- GLFW.getWindowSize win

  -- Make sure we use integral coordinates here so that we don't run into
  -- platform-dependent differences.
  let cx :: Double
      cy :: Double
      (cx, cy) = (fromIntegral $ w `div` 2, fromIntegral $ h `div` 2)

  -- !HACK! Poll events seems to be necessary on OS X,
  -- before /and/ after glfwSetCursorPos, otherwise, the
  -- windowing system likely never receives the cursor update. This is
  -- reflected in the C version of GLFW as well, we just call it here in
  -- order to have a more robust test.

  GLFW.pollEvents
  GLFW.setCursorPos win cx cy
  GLFW.pollEvents -- !HACK! see comment above

  (cx', cy') <- GLFW.getCursorPos win
  cx' @?= cx
  cy' @?= cy
  GLFW.hideWindow win

test_getWindowFocused :: GLFW.Window -> IO ()
test_getWindowFocused win = do
    fs <- GLFW.getWindowFocused win
    fs @?= False

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
    assertBool "" $ all (`between` (0, 20)) [v0, v1]

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
test_waitEvents = GLFW.postEmptyEvent >> GLFW.waitEvents

test_waitEventsTimeout :: IO ()
test_waitEventsTimeout =
    -- to not slow down the test too much we set the timeout to 0.001 second :
    GLFW.waitEventsTimeout 0.001

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

test_getKeyName :: IO ()
test_getKeyName =
  forM_ (zip [GLFW.Key'Slash, GLFW.Key'Period] ["/", "."]) $ \(k, n) -> do
    name <- GLFW.getKeyName k 0
    case name of
      Nothing -> return ()
      Just s -> s @?= n

test_getGamepadState :: IO ()
test_getGamepadState =
  forM_ joysticks $ \js -> do
    mst <- GLFW.getGamepadState js
    case mst of
      (Just st) -> do
        GLFW.joystickIsGamepad js >>= assertBool "Is gamepad"
        GLFW.getGamepadName js >>= assertBool "Gamepad has name" . not . null
        forM_ [minBound..maxBound] $ assertBool "Button not pressed"
                                   . (== GLFW.GamepadButtonState'Released)
                                   . GLFW.getButtonState st
      _ -> return ()

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

test_getTimerValue :: IO ()
test_getTimerValue = GLFW.getTimerValue >>= assertBool "" . (> 0)

test_getTimerFrequency :: IO ()
test_getTimerFrequency = GLFW.getTimerFrequency >>= assertBool "" . (> 0)

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
        giveItTime
        result <- GLFW.getClipboardString win

        err <- GLFW.getError
        return $ case err of
          Just (GLFW.Error'FormatUnavailable, _) -> Just s
          _ -> result

--------------------------------------------------------------------------------

{-# ANN module "HLint: ignore Use camelCase" #-}
