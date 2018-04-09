{-|

Threading restrictions which apply to the C version of GLFW still apply when
writing @GLFW-b@ programs. See
<http://www.glfw.org/docs/3.1/intro.html#thread_safety GLFW thread safety documentation>
(applies here).

Current context restructions which apply to the C version of GLFW still apply.
See <http://www.glfw.org/docs/3.1/context.html#context_current  GLFW current context documentation>
(applies here).

@GLFW-b@ wraps callbacks and schedules them to be run after 'pollEvents' and
'waitEvents' in the normal GHC runtime where they aren't subject to the usual
GLFW reentrancy restrictions. See
<http://www.glfw.org/docs/3.1/intro.html#reentrancy GLFW reentrancy documentation>
(does not apply here).

-}
module Graphics.UI.GLFW
  ( -- * Error handling
    Error (..)
    --
  , setErrorCallback, ErrorCallback

    -- * Initialization and version information
  , Version (..)
    --
  , init
  , terminate
  , getVersion
  , getVersionString

    -- * Monitor handling
  , Monitor
  , MonitorState (..)
  , VideoMode    (..)
  , GammaRamp    (gammaRampRed, gammaRampGreen, gammaRampBlue)
  , makeGammaRamp
    --
  , getMonitors
  , getPrimaryMonitor
  , getMonitorPos
  , getMonitorPhysicalSize
  , getMonitorName
  , setMonitorCallback, MonitorCallback
  , getVideoModes
  , getVideoMode
  , setGamma
  , getGammaRamp
  , setGammaRamp

    -- * Window handling
  , Window
  , WindowHint        (..)
  , FocusState        (..)
  , IconifyState      (..)
  , ContextRobustness (..)
  , OpenGLProfile     (..)
  , ClientAPI         (..)
    --
  , defaultWindowHints
  , windowHint
  , createWindow
  , destroyWindow
  , windowShouldClose
  , setWindowShouldClose
  , setWindowTitle
  , getWindowPos
  , setWindowPos
  , getWindowSize
  , setWindowSize
  , getFramebufferSize
  , iconifyWindow
  , restoreWindow
  , showWindow
  , hideWindow
  , getWindowMonitor
  , setCursorPos
    -- related to c'glfwGetWindowAttrib --.
  , getWindowFocused                   -- |
  , getWindowIconified                 -- |
  , getWindowResizable                 -- |
  , getWindowDecorated                 -- |
  , getWindowVisible                   -- |
  , getWindowClientAPI                 -- |
  , getWindowContextVersionMajor       -- |
  , getWindowContextVersionMinor       -- |
  , getWindowContextVersionRevision    -- |
  , getWindowContextRobustness         -- |
  , getWindowOpenGLForwardCompat       -- |
  , getWindowOpenGLDebugContext        -- |
  , getWindowOpenGLProfile  --------------'
  , setWindowPosCallback,       WindowPosCallback
  , setWindowSizeCallback,      WindowSizeCallback
  , setWindowCloseCallback,     WindowCloseCallback
  , setWindowRefreshCallback,   WindowRefreshCallback
  , setWindowFocusCallback,     WindowFocusCallback
  , setWindowIconifyCallback,   WindowIconifyCallback
  , setFramebufferSizeCallback, FramebufferSizeCallback
  , pollEvents
  , waitEvents
  , postEmptyEvent

    -- * Input handling
  , Key                         (..)
  , KeyState                    (..)
  , Joystick                    (..)
  , JoystickButtonState         (..)
  , MouseButton                 (..)
  , MouseButtonState            (..)
  , CursorState                 (..)
  , CursorInputMode             (..)
  , StickyKeysInputMode         (..)
  , StickyMouseButtonsInputMode (..)
  , ModifierKeys                (..)
  , Image                       (..)
  , Cursor                      (..)
  , StandardCursorShape         (..)
    --
    -- related to c'glfwSetInputMode ----.
  , getCursorInputMode                -- |
  , setCursorInputMode                -- |
  , getStickyKeysInputMode            -- |
  , setStickyKeysInputMode            -- |
  , getStickyMouseButtonsInputMode    -- |
  , setStickyMouseButtonsInputMode  -----'
  , getKey
  , getMouseButton
  , getCursorPos
  , setKeyCallback,         KeyCallback
  , setCharCallback,        CharCallback
  , setMouseButtonCallback, MouseButtonCallback
  , setCursorPosCallback,   CursorPosCallback
  , setCursorEnterCallback, CursorEnterCallback
  , createCursor
  , createStandardCursor
  , setCursor
  , destroyCursor
  , setScrollCallback,      ScrollCallback
  , setDropCallback,        DropCallback
  , joystickPresent
  , getJoystickAxes
  , getJoystickButtons
  , getJoystickName

    -- * Time
  , getTime
  , setTime

    -- * Context
  , makeContextCurrent
  , getCurrentContext
  , swapBuffers
  , swapInterval
  , extensionSupported

    -- * Clipboard
  , getClipboardString
  , setClipboardString

    -- * Vulkan-related functions
  , vulkanSupported
  , getRequiredInstanceExtensions
  , getInstanceProcAddress
  , getPhysicalDevicePresentationSupport
  , createWindowSurface
  ) where

--------------------------------------------------------------------------------

import Prelude hiding (init)

import Control.Monad         (when, liftM)
import Data.IORef            (IORef, atomicModifyIORef, newIORef, readIORef)
import Data.Word             (Word32)
import Foreign.C.String      (peekCString, withCString, CString)
import Foreign.C.Types       (CUInt, CUShort)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (advancePtr, allocaArray, peekArray, withArray)
import Foreign.Ptr           (FunPtr, freeHaskellFunPtr, nullFunPtr, nullPtr
                             ,Ptr)
import Foreign.StablePtr
import Foreign.Storable      (Storable (..))
import System.IO.Unsafe      (unsafePerformIO)

import Graphics.UI.GLFW.C
import Graphics.UI.GLFW.Types

import Bindings.GLFW

--------------------------------------------------------------------------------

-- We store FunPtrs from mk'GLFW*fun in these stored*Fun IORefs. Initialized
-- with unsafePerformIO, they are basically mutable global variables.

storedErrorFun           :: IORef C'GLFWerrorfun
storedMonitorFun         :: IORef C'GLFWmonitorfun

storedErrorFun           = unsafePerformIO $ newIORef nullFunPtr
storedMonitorFun         = unsafePerformIO $ newIORef nullFunPtr

-- These NOINLINE pragmas are due to use of unsafePerformIO.
-- See http://hackage.haskell.org/packages/archive/base/latest/doc/html/System-IO-Unsafe.html#v:unsafePerformIO .

{-# NOINLINE storedErrorFun           #-}
{-# NOINLINE storedMonitorFun         #-}

setWindowCallback
  :: (c -> IO (FunPtr c))                    -- wf   wrapper function
  -> (h -> c)                                -- af   adapter function
  -> (FunPtr c -> IO (FunPtr c))             -- gf   c'glfwSet*Callback function
  -> (WindowCallbacks -> IORef (FunPtr c))   -- ior  accessor for storage location
  -> Window                                  -- win  window
  -> Maybe h                                 -- mcb  Haskell callback
  -> IO ()
setWindowCallback wr af gf ior win mcb = do
    pcallbacks <- castPtrToStablePtr `liftM` c'glfwGetWindowUserPointer (unWindow win)
    callbacks <- deRefStablePtr pcallbacks
    setCallback wr af gf (ior callbacks) mcb

setCallback
  :: (c -> IO (FunPtr c))          -- wf   wrapper function
  -> (h -> c)                      -- af   adapter function
  -> (FunPtr c -> IO (FunPtr c))   -- gf   c'glfwSet*Callback function
  -> IORef (FunPtr c)              -- ior  storage location
  -> Maybe h                       -- mcb  Haskell callback
  -> IO ()
setCallback wf af gf ior mcb = do
    -- If mcb is Just, make ccb the FunPtr of the adapted callback. Otherwise a
    -- null FunPtr.
    ccb <- maybe (return nullFunPtr) (wf . af) mcb
    -- Call the GLFW callback-setting function.
    _ <- gf ccb
    -- Store it.
    storeCallback ior ccb

storeCallback :: IORef (FunPtr a) -> FunPtr a -> IO ()
storeCallback ior new = do
    -- Store the new FunPtr, retrieve the previous one.
    prev <- atomicModifyIORef ior (\cur -> (new, cur))
    -- Free the old FunPtr if necessary.
    when (prev /= nullFunPtr) $ freeHaskellFunPtr prev

--------------------------------------------------------------------------------

-- | The error code and also a human-readable error message.
type ErrorCallback           = Error -> String                                           -> IO ()
-- | Fires when the window position changes.
type WindowPosCallback       = Window -> Int -> Int                                      -> IO ()
-- | Fires when the window is resized (in Screen Coordinates, which might not map 1:1 with pixels).
type WindowSizeCallback      = Window -> Int -> Int                                      -> IO ()
-- | Fires when the user is attempting to close the window
type WindowCloseCallback     = Window                                                    -> IO ()
-- | Fires when the contents of the window are damaged and they must be refreshed.
type WindowRefreshCallback   = Window                                                    -> IO ()
-- | Fires when the window gains or loses input focus.
type WindowFocusCallback     = Window -> FocusState                                      -> IO ()
-- | Fires when the window is iconified (minimized) or not.
type WindowIconifyCallback   = Window -> IconifyState                                    -> IO ()
-- | Fires when the size of the framebuffer for the window changes (in Pixels).
type FramebufferSizeCallback = Window -> Int -> Int                                      -> IO ()
-- | Fires whenever a mouse button is clicked.
type MouseButtonCallback     = Window -> MouseButton -> MouseButtonState -> ModifierKeys -> IO ()
-- | Fires every time the cursor position changes. Sub-pixel accuracy is used, when available.
type CursorPosCallback       = Window -> Double -> Double                                -> IO ()
-- | Fires when the cursor enters or exits the client area of the window.
type CursorEnterCallback     = Window -> CursorState                                     -> IO ()
-- | Fires when the user scrolls the mouse wheel or via touch gesture.
type ScrollCallback          = Window -> Double -> Double                                -> IO ()
-- | Fires for each press or repeat of keyboard keys (regardless of if it has textual meaning or not, eg Shift)
type KeyCallback             = Window -> Key -> Int -> KeyState -> ModifierKeys          -> IO ()
-- | Fires when a complete character codepoint is typed by the user, Shift then 'b' generates "B".
type CharCallback            = Window -> Char                                            -> IO ()
-- | Fires when a monitor is connected or disconnected.
type MonitorCallback         = Monitor -> MonitorState                                   -> IO ()

-- 3.1 additions

--------------------------------------------------------------------------------
-- CB scheduling

data ScheduledCallbacks = ScheduledCallbacks
  { _forward :: [IO ()] -- Execution iterates this list
  , _backward :: [IO ()] -- New schedules prepend here
  }

storedScheduledCallbacks :: IORef ScheduledCallbacks
storedScheduledCallbacks = unsafePerformIO . newIORef $ ScheduledCallbacks [] []

-- This NOINLINE pragma is due to use of unsafePerformIO.
-- See http://hackage.haskell.org/packages/archive/base/latest/doc/html/System-IO-Unsafe.html#v:unsafePerformIO .

{-# NOINLINE storedScheduledCallbacks #-}

-- This is provided in newer "base" versions. To avoid depending on
-- it, it's reimplemented here. Should remove if/when compatibility
-- with older base is not an issue:
atomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef' ref f = do
    b <- atomicModifyIORef ref
            (\x -> let (a, b) = f x
                    in (a, a `seq` b))
    b `seq` return b

schedule :: IO () -> IO ()
schedule act =
  atomicModifyIORef' storedScheduledCallbacks $
  \(ScheduledCallbacks oldForward oldBackward) ->
  (ScheduledCallbacks oldForward (act : oldBackward), ())

splitFirst :: [a] -> (Maybe a, [a])
splitFirst [] = (Nothing, [])
splitFirst (x:xs) = (Just x, xs)

getNextScheduled :: IO (Maybe (IO ()))
getNextScheduled =
  atomicModifyIORef storedScheduledCallbacks $
  \(ScheduledCallbacks oldForward oldBackward) ->
  case oldForward of
    [] ->
      let (mCb, newForward) = splitFirst (reverse oldBackward)
      in (ScheduledCallbacks newForward [], mCb)
    (cb:rest) ->                -- Eat forward first
      (ScheduledCallbacks rest oldBackward, Just cb)

executeScheduled :: IO ()
executeScheduled = do
  mcb <- getNextScheduled
  case mcb of
    Nothing -> return ()
    Just cb -> cb >> executeScheduled

--------------------------------------------------------------------------------
-- Error handling

-- | Can (and probably should) be used before GLFW initialization.
-- See <http://www.glfw.org/docs/3.1/group__init.html#gaa5d796c3cf7c1a7f02f845486333fb5f glfwSetErrorCallback>
setErrorCallback :: Maybe ErrorCallback -> IO ()
setErrorCallback = setCallback
    mk'GLFWerrorfun
    (\cb a0 a1 -> do
        s <- peekCString a1
        schedule $ cb (fromC a0) s)
    c'glfwSetErrorCallback
    storedErrorFun

--------------------------------------------------------------------------------
-- Initialization and version information

-- | Attempts to initialize the GLFW library. When the library is not initialized, the only
-- allowed functions to call are 'getVersion', 'getVersionString', 'setErrorCallback',
-- 'init', and 'terminate'. Returns if the initialization was successful or not.
-- See <http://www.glfw.org/docs/3.1/group__init.html#ga317aac130a235ab08c6db0834907d85e glfwInit>
-- and <http://www.glfw.org/docs/3.1/intro.html#intro_init Initialization and Termination>
init :: IO Bool
init =
    fromC `fmap` c'glfwInit

-- | Cleans up GLFW and puts the library into an uninitialized state.
-- Once you call this, you must initilize the library again.
-- Warning: No window's context may be current in another thread when this is called.
-- See <http://www.glfw.org/docs/3.1/group__init.html#gaaae48c0a18607ea4a4ba951d939f0901 glfwTerminate>
-- and <http://www.glfw.org/docs/3.1/intro.html#intro_init Initialization and Termination>
terminate :: IO ()
terminate = do
    c'glfwTerminate
    -- Free all stored FunPtrs.
    storeCallback storedErrorFun           nullFunPtr
    storeCallback storedMonitorFun         nullFunPtr

-- | Gets the version of the GLFW library that's being used with the current program.
-- See <http://www.glfw.org/docs/3.1/group__init.html#ga9f8ffaacf3c269cc48eafbf8b9b71197 glfwGetVersion>
getVersion :: IO Version
getVersion =
    allocaArray 3 $ \p -> do
        let p0 = p
            p1 = p `advancePtr` 1
            p2 = p `advancePtr` 2
        c'glfwGetVersion p0 p1 p2
        v0 <- fromC `fmap` peek p0
        v1 <- fromC `fmap` peek p1
        v2 <- fromC `fmap` peek p2
        return $ Version v0 v1 v2

-- | Gets the compile-time version string of the GLFW library binary.
-- Gives extra info like platform and compile time options used, but you should not
-- attempt to parse this to get the GLFW version number. Use 'getVersion' instead.
-- See <http://www.glfw.org/docs/3.1/group__init.html#ga23d47dc013fce2bf58036da66079a657 glfwGetVersionString>
getVersionString :: IO (Maybe String)
getVersionString = do
    p'vs <- c'glfwGetVersionString
    if p'vs /= nullPtr
      then Just `fmap` peekCString p'vs
      else return Nothing

--------------------------------------------------------------------------------
-- Monitor handling

-- | Gets the list of available monitors, if possible.
-- See <http://www.glfw.org/docs/latest/group__monitor.html#ga3fba51c8bd36491d4712aa5bd074a537 glfwGetMonitors>
getMonitors :: IO (Maybe [Monitor])
getMonitors =
    alloca $ \p'n -> do
        p'mon <- c'glfwGetMonitors p'n
        n <- fromC `fmap` peek p'n
        if p'mon == nullPtr || n <= 0
          then return Nothing
          else (Just . map fromC) `fmap` peekArray n p'mon

-- | Gets the primary monitor.
-- See <http://www.glfw.org/docs/latest/group__monitor.html#ga721867d84c6d18d6790d64d2847ca0b1 glfwGetPrimaryMonitor>
getPrimaryMonitor :: IO (Maybe Monitor)
getPrimaryMonitor = do
    p'mon <- c'glfwGetPrimaryMonitor
    return $
      if p'mon == nullPtr
        then Nothing
        else Just $ fromC p'mon

-- | Gets the position of the specified monitor within the coordinate space.
-- See <http://www.glfw.org/docs/latest/group__monitor.html#ga102f54e7acc9149edbcf0997152df8c9 glfwGetMonitorPos>
getMonitorPos :: Monitor -> IO (Int, Int)
getMonitorPos mon =
    allocaArray 2 $ \p -> do
        let p'x = p
            p'y = p `advancePtr` 1
        c'glfwGetMonitorPos (toC mon) p'x p'y
        x <- fromC `fmap` peek p'x
        y <- fromC `fmap` peek p'y
        return (x, y)

-- | The physical width and height of the monitor.
-- See <http://www.glfw.org/docs/latest/group__monitor.html#ga7d8bffc6c55539286a6bd20d32a8d7ea glfwGetMonitorPhysicalSize>
getMonitorPhysicalSize :: Monitor -> IO (Int, Int)
getMonitorPhysicalSize mon =
    allocaArray 2 $ \p -> do
        let p'w = p
            p'h = p `advancePtr` 1
        c'glfwGetMonitorPhysicalSize (toC mon) p'w p'h
        w <- fromC `fmap` peek p'w
        h <- fromC `fmap` peek p'h
        return (w, h)

-- | A human-readable name for the monitor specified.
-- See <http://www.glfw.org/docs/latest/group__monitor.html#ga79a34ee22ff080ca954a9663e4679daf getMonitorName>
getMonitorName :: Monitor -> IO (Maybe String)
getMonitorName mon = do
    p'name <- c'glfwGetMonitorName (toC mon)
    if p'name == nullPtr
      then return Nothing
      else Just `fmap` peekCString p'name

-- | Sets a callback for when a monitor is connected or disconnected.
-- See <http://www.glfw.org/docs/latest/group__monitor.html#gac3fe0f647f68b731f99756cd81897378 glfwSetMonitorCallback>
setMonitorCallback :: Maybe MonitorCallback -> IO ()
setMonitorCallback = setCallback
    mk'GLFWmonitorfun
    (\cb a0 a1 -> schedule $ cb (fromC a0) (fromC a1))
    c'glfwSetMonitorCallback
    storedMonitorFun

-- | Obtains the possible video modes of the monitor.
-- See <http://www.glfw.org/docs/latest/group__monitor.html#ga820b0ce9a5237d645ea7cbb4bd383458 glfwGetVideoModes>
getVideoModes :: Monitor -> IO (Maybe [VideoMode])
getVideoModes mon =
    alloca $ \p'n -> do
        p'vms <- c'glfwGetVideoModes (toC mon) p'n
        n <- fromC `fmap` peek p'n
        if p'vms == nullPtr || n <= 0
          then return Nothing
          else (Just . map fromC) `fmap` peekArray n p'vms

-- | Gets the active video mode of the monitor.
-- See <http://www.glfw.org/docs/latest/group__monitor.html#gafc1bb972a921ad5b3bd5d63a95fc2d52 glfwGetVideoMode>
getVideoMode :: Monitor -> IO (Maybe VideoMode)
getVideoMode mon = do
    p'vm <- c'glfwGetVideoMode (toC mon)
    if p'vm == nullPtr
      then return Nothing
      else (Just . fromC) `fmap` peek p'vm

-- | Sets the gamma of a monitor.
-- See <http://www.glfw.org/docs/latest/group__monitor.html#ga6ac582625c990220785ddd34efa3169a glfwSetGamma>
setGamma :: Monitor -> Double -> IO ()
setGamma mon e =
    c'glfwSetGamma (toC mon) (toC e)

-- | Gets the gamma ramp in use with the monitor.
-- See <http://www.glfw.org/docs/latest/group__monitor.html#gab7c41deb2219bde3e1eb756ddaa9ec80 glfwGetGammaRamp>
getGammaRamp :: Monitor -> IO (Maybe GammaRamp)
getGammaRamp m = do
    p'ggr <- c'glfwGetGammaRamp (toC m)
    if p'ggr == nullPtr
      then return Nothing
      else do
          ggr <- peek p'ggr
          let p'rs = c'GLFWgammaramp'red   ggr
              p'gs = c'GLFWgammaramp'green ggr
              p'bs = c'GLFWgammaramp'blue  ggr
              cn   = c'GLFWgammaramp'size  ggr
              n    = fromC cn
          if n == 0 || nullPtr `elem` [p'rs, p'gs, p'bs]
            then return Nothing
            else do
                rs <- map fromC `fmap` peekArray n p'rs
                gs <- map fromC `fmap` peekArray n p'gs
                bs <- map fromC `fmap` peekArray n p'bs
                return $ Just GammaRamp
                  { gammaRampRed   = rs
                  , gammaRampGreen = gs
                  , gammaRampBlue  = bs
                  }

-- | Assigns a gamma ramp to use with the given monitor.
-- See <http://www.glfw.org/docs/latest/group__monitor.html#ga583f0ffd0d29613d8cd172b996bbf0dd glfwSetGammaRamp>
setGammaRamp :: Monitor -> GammaRamp -> IO ()
setGammaRamp mon gr =
    let rs = map toC $ gammaRampRed   gr :: [CUShort]
        gs = map toC $ gammaRampGreen gr :: [CUShort]
        bs = map toC $ gammaRampBlue  gr :: [CUShort]
        -- GammaRamp's smart constructor ensures that the RGB lists all have
        -- equal length, so just use the number of reds.
        cn = toC $ length rs :: CUInt
    in alloca       $ \p'ggr ->
       withArray rs $ \p'rs  ->
       withArray gs $ \p'gs  ->
       withArray bs $ \p'bs  -> do
          let ggr = C'GLFWgammaramp
                      { c'GLFWgammaramp'red   = p'rs
                      , c'GLFWgammaramp'green = p'gs
                      , c'GLFWgammaramp'blue  = p'bs
                      , c'GLFWgammaramp'size  = cn
                      }
          poke p'ggr ggr
          c'glfwSetGammaRamp (toC mon) p'ggr

--------------------------------------------------------------------------------
-- Window handling

-- | Sets all the window hints to default.
-- See <http://www.glfw.org/docs/latest/group__window.html#gaa77c4898dfb83344a6b4f76aa16b9a4a glfwDefaultWindowHints>
defaultWindowHints :: IO ()
defaultWindowHints =
    c'glfwDefaultWindowHints

-- | Hints something to the GLFW windowing system.
-- See <http://www.glfw.org/docs/latest/group__window.html#ga7d9c8c62384b1e2821c4dc48952d2033 glfwWindowHint>
windowHint :: WindowHint -> IO ()
windowHint wh =
    let (t, v) = unpack
    in c'glfwWindowHint t v
  where
    unpack = case wh of
      (WindowHint'Resizable           x) -> (c'GLFW_RESIZABLE,             toC x)
      (WindowHint'Visible             x) -> (c'GLFW_VISIBLE,               toC x)
      (WindowHint'Decorated           x) -> (c'GLFW_DECORATED,             toC x)
      (WindowHint'RedBits             x) -> (c'GLFW_RED_BITS,              toC x)
      (WindowHint'GreenBits           x) -> (c'GLFW_GREEN_BITS,            toC x)
      (WindowHint'BlueBits            x) -> (c'GLFW_BLUE_BITS,             toC x)
      (WindowHint'AlphaBits           x) -> (c'GLFW_ALPHA_BITS,            toC x)
      (WindowHint'DepthBits           x) -> (c'GLFW_DEPTH_BITS,            toC x)
      (WindowHint'StencilBits         x) -> (c'GLFW_STENCIL_BITS,          toC x)
      (WindowHint'AccumRedBits        x) -> (c'GLFW_ACCUM_RED_BITS,        toC x)
      (WindowHint'AccumGreenBits      x) -> (c'GLFW_ACCUM_GREEN_BITS,      toC x)
      (WindowHint'AccumBlueBits       x) -> (c'GLFW_ACCUM_BLUE_BITS,       toC x)
      (WindowHint'AccumAlphaBits      x) -> (c'GLFW_ACCUM_ALPHA_BITS,      toC x)
      (WindowHint'AuxBuffers          x) -> (c'GLFW_AUX_BUFFERS,           toC x)
      (WindowHint'Samples             x) -> (c'GLFW_SAMPLES,               toC x)
      (WindowHint'RefreshRate         x) -> (c'GLFW_REFRESH_RATE,          toC x)
      (WindowHint'Stereo              x) -> (c'GLFW_STEREO,                toC x)
      (WindowHint'sRGBCapable         x) -> (c'GLFW_SRGB_CAPABLE,          toC x)
      (WindowHint'ClientAPI           x) -> (c'GLFW_CLIENT_API,            toC x)
      (WindowHint'ContextVersionMajor x) -> (c'GLFW_CONTEXT_VERSION_MAJOR, toC x)
      (WindowHint'ContextVersionMinor x) -> (c'GLFW_CONTEXT_VERSION_MINOR, toC x)
      (WindowHint'ContextRobustness   x) -> (c'GLFW_CONTEXT_ROBUSTNESS,    toC x)
      (WindowHint'OpenGLForwardCompat x) -> (c'GLFW_OPENGL_FORWARD_COMPAT, toC x)
      (WindowHint'OpenGLDebugContext  x) -> (c'GLFW_OPENGL_DEBUG_CONTEXT,  toC x)
      (WindowHint'OpenGLProfile       x) -> (c'GLFW_OPENGL_PROFILE,        toC x)

-- | Creates a new window.
-- Note: If running in GHCI don't forget to `:set -fno-ghci-sandbox` or you
-- may run into an assertion failure, segfault or other nasty crash.
-- See <http://www.glfw.org/docs/latest/group__window.html#ga5c336fddf2cbb5b92f65f10fb6043344 glfwCreateWindow>
createWindow :: Int -- ^ Desired width for the window.
             -> Int -- ^ Desired height for the window.
             -> String -- ^ Desired title for the window.
             -> Maybe Monitor -- ^ Monitor to use in fullscreen mode.
             -> Maybe Window  -- ^ Window for context object sharing, see
                              -- <http://www.glfw.org/docs/latest/context.html#context_sharing here>.
             -> IO (Maybe Window)
createWindow w h title mmon mwin =
    withCString title $ \ptitle -> do
        charFun             <- newIORef nullFunPtr
        cursorEnterFun      <- newIORef nullFunPtr
        cursorPosFun        <- newIORef nullFunPtr
        framebufferSizeFun  <- newIORef nullFunPtr
        keyFun              <- newIORef nullFunPtr
        mouseButtonFun      <- newIORef nullFunPtr
        scrollFun           <- newIORef nullFunPtr
        windowCloseFun      <- newIORef nullFunPtr
        windowFocusFun      <- newIORef nullFunPtr
        windowIconifyFun    <- newIORef nullFunPtr
        windowPosFun        <- newIORef nullFunPtr
        windowRefreshFun    <- newIORef nullFunPtr
        windowSizeFun       <- newIORef nullFunPtr
        dropFun             <- newIORef nullFunPtr
        let callbacks = WindowCallbacks
              { storedCharFun             = charFun
              , storedCursorEnterFun      = cursorEnterFun
              , storedCursorPosFun        = cursorPosFun
              , storedFramebufferSizeFun  = framebufferSizeFun
              , storedKeyFun              = keyFun
              , storedMouseButtonFun      = mouseButtonFun
              , storedScrollFun           = scrollFun
              , storedWindowCloseFun      = windowCloseFun
              , storedWindowFocusFun      = windowFocusFun
              , storedWindowIconifyFun    = windowIconifyFun
              , storedWindowPosFun        = windowPosFun
              , storedWindowRefreshFun    = windowRefreshFun
              , storedWindowSizeFun       = windowSizeFun
              , storedDropFun             = dropFun
              }
        p'win <- c'glfwCreateWindow
          (toC w)
          (toC h)
          ptitle
          (maybe nullPtr toC mmon)
          (maybe nullPtr toC mwin)
        if p'win == nullPtr
          then return Nothing
          else do callbackPtr <- newStablePtr callbacks
                  c'glfwSetWindowUserPointer p'win (castStablePtrToPtr callbackPtr)
                  return $ Just $ fromC p'win

-- | Cleans up a window and all associated resources
-- See <http://www.glfw.org/docs/latest/group__window.html#gacdf43e51376051d2c091662e9fe3d7b2 glfwDestroyWindow>
destroyWindow :: Window -> IO ()
destroyWindow win = do
    pcb <- castPtrToStablePtr `liftM` c'glfwGetWindowUserPointer (toC win)
    cbs <- deRefStablePtr pcb
    c'glfwDestroyWindow (toC win)

    let free callback = do funptr <- readIORef (callback cbs)
                           when (funptr /= nullFunPtr) $ freeHaskellFunPtr funptr
    free storedCharFun
    free storedCursorEnterFun
    free storedCursorPosFun
    free storedFramebufferSizeFun
    free storedKeyFun
    free storedMouseButtonFun
    free storedScrollFun
    free storedWindowCloseFun
    free storedWindowFocusFun
    free storedWindowIconifyFun
    free storedWindowPosFun
    free storedWindowRefreshFun
    free storedWindowSizeFun
    freeStablePtr pcb

-- | If the window should close or not.
-- See <http://www.glfw.org/docs/latest/group__window.html#ga24e02fbfefbb81fc45320989f8140ab5 glfwWindowShouldClose>
windowShouldClose :: Window -> IO Bool
windowShouldClose win =
    fromC `fmap` c'glfwWindowShouldClose (toC win)

-- | Sets if the window should close or not.
-- See <http://www.glfw.org/docs/latest/group__window.html#ga49c449dde2a6f87d996f4daaa09d6708 glfwSetWindowShouldClose>
setWindowShouldClose :: Window -> Bool -> IO ()
setWindowShouldClose win b =
    c'glfwSetWindowShouldClose (toC win) (toC b)

-- | Sets the Title string of the window.
-- See <http://www.glfw.org/docs/latest/group__window.html#ga5d877f09e968cef7a360b513306f17ff glfwSetWindowTitle>
setWindowTitle :: Window -> String -> IO ()
setWindowTitle win title =
    withCString title $ c'glfwSetWindowTitle (toC win)

-- | Gets the window's position (in Screen Coordinates).
-- See <http://www.glfw.org/docs/latest/group__window.html#ga73cb526c000876fd8ddf571570fdb634 glfwGetWindowPos>
getWindowPos :: Window -> IO (Int, Int)
getWindowPos win =
    allocaArray 2 $ \p -> do
        let p'x = p
            p'y = p `advancePtr` 1
        c'glfwGetWindowPos (toC win) p'x p'y
        x <- fromC `fmap` peek p'x
        y <- fromC `fmap` peek p'y
        return (x, y)

-- | Sets the window's position (in Screen Coordinates).
-- See <http://www.glfw.org/docs/latest/group__window.html#ga1abb6d690e8c88e0c8cd1751356dbca8 glfwSetWindowPos>
setWindowPos :: Window -> Int -> Int -> IO ()
setWindowPos win x y =
    c'glfwSetWindowPos (toC win) (toC x) (toC y)

-- | Gets the size of the window (in Screen Coordinates).
-- See <http://www.glfw.org/docs/latest/group__window.html#gaeea7cbc03373a41fb51cfbf9f2a5d4c6 glfwGetWindowSize>
getWindowSize :: Window -> IO (Int, Int)
getWindowSize win =
    allocaArray 2 $ \p -> do
        let p'w = p
            p'h = p `advancePtr` 1
        c'glfwGetWindowSize (toC win) p'w p'h
        w <- fromC `fmap` peek p'w
        h <- fromC `fmap` peek p'h
        return (w, h)

-- | Sets the size of the client area for the window (in Screen Coordinates).
-- See <http://www.glfw.org/docs/latest/group__window.html#ga371911f12c74c504dd8d47d832d095cb glfwSetWindowSize>
setWindowSize :: Window -> Int -> Int -> IO ()
setWindowSize win w h =
    c'glfwSetWindowSize (toC win) (toC w) (toC h)

-- | The size of the framebuffer (in Pixels)
-- See <http://www.glfw.org/docs/latest/group__window.html#ga0e2637a4161afb283f5300c7f94785c9 glfwGetFramebufferSize>
getFramebufferSize :: Window -> IO (Int, Int)
getFramebufferSize win =
    allocaArray 2 $ \p -> do
        let p'w = p
            p'h = p `advancePtr` 1
        c'glfwGetFramebufferSize (toC win) p'w p'h
        w <- fromC `fmap` peek p'w
        h <- fromC `fmap` peek p'h
        return (w, h)

-- | Iconifies (minimizes) the window.
-- See <http://www.glfw.org/docs/latest/group__window.html#ga1bb559c0ebaad63c5c05ad2a066779c4 glfwIconifyWindow>
iconifyWindow :: Window -> IO ()
iconifyWindow =
    c'glfwIconifyWindow . toC

-- | Restores the window from an iconified/minimized state.
-- See <http://www.glfw.org/docs/latest/group__window.html#ga52527a5904b47d802b6b4bb519cdebc7 glfwRestoreWindow>
restoreWindow :: Window -> IO ()
restoreWindow =
    c'glfwRestoreWindow . toC

-- | Shows the window.
-- See <http://www.glfw.org/docs/latest/group__window.html#ga61be47917b72536a148300f46494fc66 glfwShowWindow>
showWindow :: Window -> IO ()
showWindow =
    c'glfwShowWindow . toC

-- | Hides the window.
-- See <http://www.glfw.org/docs/latest/group__window.html#ga49401f82a1ba5f15db5590728314d47c glfwHideWindow>
hideWindow :: Window -> IO ()
hideWindow =
    c'glfwHideWindow . toC

-- | Gets the monitor that this window is running on.
-- See <http://www.glfw.org/docs/latest/group__window.html#gaeac25e64789974ccbe0811766bd91a16 glfwGetWindowMonitor>
getWindowMonitor :: Window -> IO (Maybe Monitor)
getWindowMonitor win = do
    p'mon <- c'glfwGetWindowMonitor (toC win)
    return $ if p'mon == nullPtr
      then Nothing
      else Just $ fromC p'mon

-- | Sets the position of the cursor within the window.
-- See <http://www.glfw.org/docs/latest/group__input.html#ga04b03af936d906ca123c8f4ee08b39e7 glfwSetCursorPos>
setCursorPos :: Window -> Double -> Double -> IO ()
setCursorPos win x y =
    c'glfwSetCursorPos (toC win) (toC x) (toC y)

-- start of functions related to c'glfwGetWindowAttrib

-- | If the window has focus or not.
-- See <http://www.glfw.org/docs/latest/group__window.html#gacccb29947ea4b16860ebef42c2cb9337 glfwGetWindowAttrib>
getWindowFocused :: Window -> IO FocusState
getWindowFocused win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_FOCUSED

-- | If the window is iconified (minimized) or not.
-- See <http://www.glfw.org/docs/latest/group__window.html#gacccb29947ea4b16860ebef42c2cb9337 glfwGetWindowAttrib>
getWindowIconified :: Window -> IO IconifyState
getWindowIconified win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_ICONIFIED

-- | If the window is resizable or not.
-- See <http://www.glfw.org/docs/latest/group__window.html#gacccb29947ea4b16860ebef42c2cb9337 glfwGetWindowAttrib>
getWindowResizable :: Window -> IO Bool
getWindowResizable win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_RESIZABLE

-- | If the window is decorated or not.
-- See <http://www.glfw.org/docs/latest/group__window.html#gacccb29947ea4b16860ebef42c2cb9337 glfwGetWindowAttrib>
getWindowDecorated :: Window -> IO Bool
getWindowDecorated win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_DECORATED

-- | If the window is visible or not.
-- See <http://www.glfw.org/docs/latest/group__window.html#gacccb29947ea4b16860ebef42c2cb9337 glfwGetWindowAttrib>
getWindowVisible :: Window -> IO Bool
getWindowVisible win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_VISIBLE

-- | The client api for this window.
-- See <http://www.glfw.org/docs/latest/group__window.html#gacccb29947ea4b16860ebef42c2cb9337 glfwGetWindowAttrib>
getWindowClientAPI :: Window -> IO ClientAPI
getWindowClientAPI win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_CLIENT_API

-- | The context's "major" version, x.0.0
-- See <http://www.glfw.org/docs/latest/group__window.html#gacccb29947ea4b16860ebef42c2cb9337 glfwGetWindowAttrib>
getWindowContextVersionMajor :: Window -> IO Int
getWindowContextVersionMajor win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_CONTEXT_VERSION_MAJOR

-- | The context's "minor" version, 0.y.0
-- See <http://www.glfw.org/docs/latest/group__window.html#gacccb29947ea4b16860ebef42c2cb9337 glfwGetWindowAttrib>
getWindowContextVersionMinor :: Window -> IO Int
getWindowContextVersionMinor win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_CONTEXT_VERSION_MINOR

-- | The context's "revision" version, 0.0.z
-- See <http://www.glfw.org/docs/latest/group__window.html#gacccb29947ea4b16860ebef42c2cb9337 glfwGetWindowAttrib>
getWindowContextVersionRevision :: Window -> IO Int
getWindowContextVersionRevision win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_CONTEXT_REVISION

-- | The context robustness of this window.
-- See <http://www.glfw.org/docs/latest/group__window.html#gacccb29947ea4b16860ebef42c2cb9337 glfwGetWindowAttrib>
getWindowContextRobustness :: Window -> IO ContextRobustness
getWindowContextRobustness win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_CONTEXT_ROBUSTNESS

-- | If this window is set for opengl to be forward compatible.
-- See <http://www.glfw.org/docs/latest/group__window.html#gacccb29947ea4b16860ebef42c2cb9337 glfwGetWindowAttrib>
getWindowOpenGLForwardCompat :: Window -> IO Bool
getWindowOpenGLForwardCompat win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_OPENGL_FORWARD_COMPAT

-- | If the window has an opengl debug context
-- See <http://www.glfw.org/docs/latest/group__window.html#gacccb29947ea4b16860ebef42c2cb9337 glfwGetWindowAttrib>
getWindowOpenGLDebugContext :: Window -> IO Bool
getWindowOpenGLDebugContext win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_OPENGL_DEBUG_CONTEXT

-- | Obtains the current opengl profile.
-- See <http://www.glfw.org/docs/latest/group__window.html#gacccb29947ea4b16860ebef42c2cb9337 glfwGetWindowAttrib>
getWindowOpenGLProfile :: Window -> IO OpenGLProfile
getWindowOpenGLProfile win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_OPENGL_PROFILE

-- end of functions related to c'glfwGetWindowAttrib

-- | Sets the callback to use when the window position changes.
-- See <http://www.glfw.org/docs/latest/group__window.html#ga2837d4d240659feb4268fcb6530a6ba1 glfwSetWindowPosCallback>
setWindowPosCallback :: Window -> Maybe WindowPosCallback -> IO ()
setWindowPosCallback win = setWindowCallback
    mk'GLFWwindowposfun
    (\cb a0 a1 a2 ->
      schedule $ cb (fromC a0) (fromC a1) (fromC a2))
    (c'glfwSetWindowPosCallback (toC win))
    storedWindowPosFun
    win

-- | Sets the callback to use when the window's size changes.
-- See <http://www.glfw.org/docs/latest/group__window.html#gaa40cd24840daa8c62f36cafc847c72b6 glfwSetWindowSizeCallback>
setWindowSizeCallback :: Window -> Maybe WindowSizeCallback -> IO ()
setWindowSizeCallback win = setWindowCallback
    mk'GLFWwindowsizefun
    (\cb a0 a1 a2 ->
      schedule $ cb (fromC a0) (fromC a1) (fromC a2))
    (c'glfwSetWindowSizeCallback (toC win))
    storedWindowSizeFun
    win

-- | Sets the callback to use when the user attempts to close the window.
-- See <http://www.glfw.org/docs/latest/group__window.html#gaade9264e79fae52bdb78e2df11ee8d6a glfwSetWindowCloseCallback>
setWindowCloseCallback :: Window -> Maybe WindowCloseCallback -> IO ()
setWindowCloseCallback win = setWindowCallback
    mk'GLFWwindowclosefun
    (. fromC)
    (c'glfwSetWindowCloseCallback (toC win))
    storedWindowCloseFun
    win

-- | Sets the callback to use when the window's data is partly dead and it should refresh.
-- See <http://www.glfw.org/docs/latest/group__window.html#ga4569b76e8ac87c55b53199e6becd97eb glfwSetWindowRefreshCallback>
setWindowRefreshCallback :: Window -> Maybe WindowRefreshCallback -> IO ()
setWindowRefreshCallback win = setWindowCallback
    mk'GLFWwindowrefreshfun
    (. fromC)
    (c'glfwSetWindowRefreshCallback (toC win))
    storedWindowRefreshFun
    win

-- | Sets the callback to use when the window gains or loses focus.
-- See <http://www.glfw.org/docs/latest/group__window.html#ga25d1c584edb375d7711c5c3548ba711f glfwSetWindowFocusCallback>
setWindowFocusCallback :: Window -> Maybe WindowFocusCallback -> IO ()
setWindowFocusCallback win = setWindowCallback
    mk'GLFWwindowfocusfun
    (\cb a0 a1 -> schedule $ cb (fromC a0) (fromC a1))
    (c'glfwSetWindowFocusCallback (toC win))
    storedWindowFocusFun
    win

-- | Sets the callback to use when the window is iconified or not (aka, minimized or not).
-- See <http://www.glfw.org/docs/latest/group__window.html#gab1ea7263081c0e073b8d5b91d6ffd367 glfwSetWindowIconifyCallback>
setWindowIconifyCallback :: Window -> Maybe WindowIconifyCallback -> IO ()
setWindowIconifyCallback win = setWindowCallback
    mk'GLFWwindowiconifyfun
    (\cb a0 a1 -> schedule $ cb (fromC a0) (fromC a1))
    (c'glfwSetWindowIconifyCallback (toC win))
    storedWindowIconifyFun
    win

-- | Sets the callback to use when the framebuffer's size changes.
-- See <http://www.glfw.org/docs/latest/group__window.html#ga3203461a5303bf289f2e05f854b2f7cf glfwSetFramebufferSizeCallback>
setFramebufferSizeCallback :: Window -> Maybe FramebufferSizeCallback -> IO ()
setFramebufferSizeCallback win = setWindowCallback
    mk'GLFWframebuffersizefun
    (\cb a0 a1 a2 -> schedule $ cb (fromC a0) (fromC a1) (fromC a2))
    (c'glfwSetFramebufferSizeCallback (toC win))
    storedFramebufferSizeFun
    win

-- | Checks for any pending events, processes them, and then immediately returns.
-- This is most useful for continual rendering, such as games.
-- See the <http://www.glfw.org/docs/3.1/input.html#events Event Processing Guide>
pollEvents :: IO ()
pollEvents = c'glfwPollEvents >> executeScheduled

-- | Waits until at least one event is in the queue then processes the queue and returns.
-- Requires at least one window to be active for it to sleep. This saves a lot of CPU, and
-- is better if you're doing only periodic rendering, such as with an editor program.
-- See the <http://www.glfw.org/docs/3.1/input.html#events Event Processing Guide>
waitEvents :: IO ()
waitEvents = c'glfwWaitEvents >> executeScheduled

-- | Creates an empty event within the event queue. Can be called from any
-- thread, so you can use this to wake up the main thread that's using
-- 'waitEvents' from a secondary thread.
-- See the <http://www.glfw.org/docs/3.1/input.html#events Event Processing Guide>
postEmptyEvent :: IO ()
postEmptyEvent = c'glfwPostEmptyEvent

--------------------------------------------------------------------------------
-- Input handling

-- start of glfw{GS}etInputMode-related functions

-- | Gets the current cursor input mode.
-- See <http://www.glfw.org/docs/3.1/group__input.html#gaa92336e173da9c8834558b54ee80563b glfwSetInputMode>
getCursorInputMode :: Window -> IO CursorInputMode
getCursorInputMode win =
    fromC `fmap` c'glfwGetInputMode (toC win) c'GLFW_CURSOR

-- | Set the cursor input mode.
-- See <http://www.glfw.org/docs/3.1/group__input.html#gaa92336e173da9c8834558b54ee80563b glfwSetInputMode>
setCursorInputMode :: Window -> CursorInputMode -> IO ()
setCursorInputMode win c =
    c'glfwSetInputMode (toC win) c'GLFW_CURSOR (toC c)

-- | Gets the current sticky keys mode.
-- See <http://www.glfw.org/docs/3.1/group__input.html#gaa92336e173da9c8834558b54ee80563b glfwSetInputMode>
getStickyKeysInputMode :: Window -> IO StickyKeysInputMode
getStickyKeysInputMode win =
    fromC `fmap` c'glfwGetInputMode (toC win) c'GLFW_STICKY_KEYS

-- | Sets if sticky keys should be used or not.
-- See <http://www.glfw.org/docs/3.1/group__input.html#gaa92336e173da9c8834558b54ee80563b glfwSetInputMode>
setStickyKeysInputMode :: Window -> StickyKeysInputMode -> IO ()
setStickyKeysInputMode win sk =
    c'glfwSetInputMode (toC win) c'GLFW_STICKY_KEYS (toC sk)

-- | Gets if sticky mouse buttons are on or not.
-- See <http://www.glfw.org/docs/3.1/group__input.html#gaa92336e173da9c8834558b54ee80563b glfwSetInputMode>
getStickyMouseButtonsInputMode :: Window -> IO StickyMouseButtonsInputMode
getStickyMouseButtonsInputMode win =
    fromC `fmap` c'glfwGetInputMode (toC win) c'GLFW_STICKY_MOUSE_BUTTONS

-- | Sets if sticky mouse buttons should be used or not.
-- See <http://www.glfw.org/docs/3.1/group__input.html#gaa92336e173da9c8834558b54ee80563b glfwSetInputMode>
setStickyMouseButtonsInputMode :: Window -> StickyMouseButtonsInputMode -> IO ()
setStickyMouseButtonsInputMode win smb =
    c'glfwSetInputMode (toC win) c'GLFW_STICKY_MOUSE_BUTTONS (toC smb)

-- end of glfw{GS}etInputMode-related functions

-- | Gets the state of the specified key. If Stickey Keys isn't enabled then it's possible for
-- keyboard polling to miss individual key presses. Use the callback to avoid this.
-- See <http://www.glfw.org/docs/3.1/group__input.html#gadd341da06bc8d418b4dc3a3518af9ad2 glfwGetKey>
getKey :: Window -> Key -> IO KeyState
getKey win k =
    fromC `fmap` c'glfwGetKey (toC win) (toC k)

-- | Gets the state of a single specified mouse button. If sticky mouse button
-- mode isn't enabled it's possible for mouse polling to miss individual mouse events. Use
-- the call back to avoid this.
-- See <http://www.glfw.org/docs/3.1/group__input.html#gac1473feacb5996c01a7a5a33b5066704 glfwGetMouseButton>
getMouseButton :: Window -> MouseButton -> IO MouseButtonState
getMouseButton win b =
    fromC `fmap` c'glfwGetMouseButton (toC win) (toC b)

-- | Returns the position, in screen coodinates, relative to the upper left.
-- If the 'CursorInputMode' is "disabled", then results are unbounded by the window size.
-- See <http://www.glfw.org/docs/3.1/group__input.html#ga01d37b6c40133676b9cea60ca1d7c0cc glfwGetCursorPos>
getCursorPos :: Window -> IO (Double, Double)
getCursorPos win =
    allocaArray 2 $ \p -> do
        let p'x = p
            p'y = p `advancePtr` 1
        c'glfwGetCursorPos (toC win) p'x p'y
        x <- fromC `fmap` peek p'x
        y <- fromC `fmap` peek p'y
        return (x, y)

-- | Assigns the given callback to use for all keyboard presses and repeats.
-- See <http://www.glfw.org/docs/3.1/group__input.html#ga7e496507126f35ea72f01b2e6ef6d155 glfwSetKeyCallback>
setKeyCallback :: Window -> Maybe KeyCallback -> IO ()
setKeyCallback win = setWindowCallback
    mk'GLFWkeyfun
    (\cb a0 a1 a2 a3 a4 ->
      schedule $ cb (fromC a0) (fromC a1) (fromC a2) (fromC a3) (fromC a4))
    (c'glfwSetKeyCallback (toC win))
    storedKeyFun
    win

-- | Sets the callback to use when the user types a character
-- See <http://www.glfw.org/docs/3.1/group__input.html#ga556239421c6a5a243c66fca28da9f742 glfwSetCharCallback>
setCharCallback :: Window -> Maybe CharCallback -> IO ()
setCharCallback win = setWindowCallback
    mk'GLFWcharfun
    (\cb a0 a1 -> schedule $ cb (fromC a0) (fromC a1))
    (c'glfwSetCharCallback (toC win))
    storedCharFun
    win

-- | Assigns the callback to run whenver a mouse button is clicked.
-- See <http://www.glfw.org/docs/3.1/group__input.html#gaef49b72d84d615bca0a6ed65485e035d glfwSetMouseButtonCallback>
setMouseButtonCallback :: Window -> Maybe MouseButtonCallback -> IO ()
setMouseButtonCallback win = setWindowCallback
    mk'GLFWmousebuttonfun
    (\cb a0 a1 a2 a3 -> schedule $ cb (fromC a0) (fromC a1) (fromC a2) (fromC a3))
    (c'glfwSetMouseButtonCallback (toC win))
    storedMouseButtonFun
    win

-- | Assigns the callback to run whenver the cursor position changes.
-- See <http://www.glfw.org/docs/3.1/group__input.html#ga7dad39486f2c7591af7fb25134a2501d glfwSetCursorPosCallback>
setCursorPosCallback :: Window -> Maybe CursorPosCallback -> IO ()
setCursorPosCallback win = setWindowCallback
    mk'GLFWcursorposfun
    (\cb a0 a1 a2 -> schedule $ cb (fromC a0) (fromC a1) (fromC a2))
    (c'glfwSetCursorPosCallback (toC win))
    storedCursorPosFun
    win

-- | Sets the callback for when the cursor enters or leaves the client area.
-- See <http://www.glfw.org/docs/3.1/input.html#cursor_enter Cursor Enter/Leave Events>
setCursorEnterCallback :: Window -> Maybe CursorEnterCallback -> IO ()
setCursorEnterCallback win = setWindowCallback
    mk'GLFWcursorenterfun
    (\cb a0 a1 -> schedule $ cb (fromC a0) (fromC a1))
    (c'glfwSetCursorEnterCallback (toC win))
    storedCursorEnterFun
    win

-- | Sets the callback to run when the user scrolls with the mouse wheel or a touch gesture.
-- See <http://www.glfw.org/docs/3.1/input.html#scrolling Scroll Input>
setScrollCallback :: Window -> Maybe ScrollCallback -> IO ()
setScrollCallback win = setWindowCallback
    mk'GLFWscrollfun
    (\cb a0 a1 a2 -> schedule $ cb (fromC a0) (fromC a1) (fromC a2))
    (c'glfwSetScrollCallback (toC win))
    storedScrollFun
    win

-- | Tests if the joystick is present at all
-- See <http://www.glfw.org/docs/3.1/group__input.html#gaffcbd9ac8ee737fcdd25475123a3c790 glfwJoystickPresent>
joystickPresent :: Joystick -> IO Bool
joystickPresent js =
    fromC `fmap` c'glfwJoystickPresent (toC js)

-- | Returns the values of all axes of the specified joystick, normalized to between -1.0 and 1.0
-- See <http://www.glfw.org/docs/3.1/group__input.html#ga6271d46a5901ec2c99601ccf4dd14731 glfwGetJoystickAxes>
getJoystickAxes :: Joystick -> IO (Maybe [Double])
getJoystickAxes js =
    alloca $ \p'n -> do
        p'axes <- c'glfwGetJoystickAxes (toC js) p'n
        n <- fromC `fmap` peek p'n
        if p'axes == nullPtr || n <= 0
          then return Nothing
          else (Just . map fromC) `fmap` peekArray n p'axes

-- | Returns a list of all joystick button states for the specified joystick.
-- See <http://www.glfw.org/docs/3.1/group__input.html#gace54cd930dcd502e118fe4021384ce1b glfwGetJoystickButtons>
getJoystickButtons :: Joystick -> IO (Maybe [JoystickButtonState])
getJoystickButtons js =
    alloca $ \p'n -> do
        p'buttons <- c'glfwGetJoystickButtons (toC js) p'n
        n <- fromC `fmap` peek p'n
        if p'buttons == nullPtr || n <= 0
          then return Nothing
          else (Just . map fromC) `fmap` peekArray n p'buttons

-- | A human-readable name for a Joystick. Not guranteed to be unique.
-- See <http://www.glfw.org/docs/3.1/group__input.html#gac8d7f6107e05cfd106cfba973ab51e19 glfwGetJoystickName>
getJoystickName :: Joystick -> IO (Maybe String)
getJoystickName js = do
    p'name <- c'glfwGetJoystickName (toC js)
    if p'name == nullPtr
      then return Nothing
      else Just `fmap` peekCString p'name

--------------------------------------------------------------------------------
-- Time

-- | Returns the time (in seconds) of the GLFW timer.
-- This is the amount of time since GLFW was initialized, unless 'setTime' was used.
-- The exact resolution is system dependent.
-- See <http://www.glfw.org/docs/3.1/group__input.html#gaa6cf4e7a77158a3b8fd00328b1720a4a glfwGetTime>
getTime :: IO (Maybe Double)
getTime = do
    t <- fromC `fmap` c'glfwGetTime
    return $ if t == 0
      then Nothing
      else Just t

-- | Sets the GLFW timer to the specified value, which is measured in seconds, and must be positive.
-- The value must also be less than ~584 years in seconds (18446744073.0).
-- After this the timer begins to count upward at the normal rate.
-- See <http://www.glfw.org/docs/3.1/group__input.html#gaf59589ef6e8b8c8b5ad184b25afd4dc0 glfwSetTime>
setTime :: Double -> IO ()
setTime =
    c'glfwSetTime . toC

--------------------------------------------------------------------------------
-- Context

-- | Makes the context of the specified window the current one for the calling thread.
-- A context can only be made current on a single thread at a time,
-- and each thread can have only a single current context at a time.
-- See <http://www.glfw.org/docs/3.1/group__context.html#ga1c04dc242268f827290fe40aa1c91157 glfwMakeContextCurrent>
makeContextCurrent :: Maybe Window -> IO ()
makeContextCurrent =
    c'glfwMakeContextCurrent . maybe nullPtr toC

-- | Obtains which window owns the current context of the calling thread.
-- See <http://www.glfw.org/docs/3.1/group__context.html#gac84759b1f6c2d271a4fea8ae89ec980d glfwGetCurrentContext>
getCurrentContext :: IO (Maybe Window)
getCurrentContext = do
    p'win <- c'glfwGetCurrentContext
    return $ if p'win == nullPtr
      then Nothing
      else Just $ fromC p'win

-- | Swaps the front and back buffers of the window.
-- See <http://www.glfw.org/docs/3.1/group__window.html#ga15a5a1ee5b3c2ca6b15ca209a12efd14 glfwSwapBuffers>
swapBuffers :: Window -> IO ()
swapBuffers =
    c'glfwSwapBuffers . toC

-- | Sets the number of screen updates that the GPU should wait after 'swapBuffers' before actually swapping the buffers.
-- Generates 'Error'NoCurrentContext' if no context is current.
-- See <http://www.glfw.org/docs/3.1/group__context.html#ga6d4e0cdf151b5e579bd67f13202994ed glfwSwapInterval>
swapInterval :: Int -> IO ()
swapInterval =
    c'glfwSwapInterval . toC

-- | If the current OpenGL or OpenGL ES context supports the extension specified.
-- Generates 'Error'NoCurrentContext' if no context is current.
-- See <http://www.glfw.org/docs/3.1/group__context.html#ga87425065c011cef1ebd6aac75e059dfa glfwExtensionSupported>
extensionSupported :: String -> IO Bool
extensionSupported ext =
    withCString ext $ \p'ext ->
      fromC `fmap` c'glfwExtensionSupported p'ext

--------------------------------------------------------------------------------
-- Clipboard
-- http://www.glfw.org/docs/3.1/input.html#clipboard

-- | The window that will own the clipboard contents, and also the clipboard string.
-- See <http://www.glfw.org/docs/3.1/group__input.html#gaba1f022c5eb07dfac421df34cdcd31dd glfwSetClipboardString>
setClipboardString :: Window -> String -> IO ()
setClipboardString win s =
    withCString s (c'glfwSetClipboardString (toC win))

-- | Obtains the contents of the system keyboard, if possible.
-- Generates 'Error'FormatUnavailable' if the system clipboard is empty or if it's not a UTF-8 string.
-- See <http://www.glfw.org/docs/3.1/group__input.html#ga5aba1d704d9ab539282b1fbe9f18bb94 glfwGetClipboardString>
getClipboardString :: Window -> IO (Maybe String)
getClipboardString win = do
    p's <- c'glfwGetClipboardString (toC win)
    if p's == nullPtr
      then return Nothing
      else Just `fmap` peekCString p's

--------------------------------------------------------------------------------
-- 3.1 additions (http://www.glfw.org/docs/latest/news.html#news_31)
--------------------------------------------------------------------------------

-- Cursor Objects
-- http://www.glfw.org/docs/3.1/input.html#cursor_object

-- | Creates a new cursor.
createCursor :: Image -- ^ The desired cursor image.
             -> Int   -- ^ The desired x-coordinate, in pixels, of the cursor
                      --   hotspot.
             -> Int   -- ^ The desired y-coordinate, in pixels, of the cursor
                      --   hotspot.
             -> IO Cursor
createCursor (Image w h pxs) x y =
    alloca        $ \p'img ->
    withArray pxs $ \p'pxs -> do
        let img = C'GLFWimage (toC w)
                              (toC h)
                              p'pxs
        poke p'img img
        Cursor `fmap` c'glfwCreateCursor p'img (toC x) (toC y)

-- | Creates a cursor with a standard shape that can be set for a window with
-- setCursor.
createStandardCursor :: StandardCursorShape -> IO Cursor
createStandardCursor = (fmap Cursor) . c'glfwCreateStandardCursor . toC

-- | Sets the cursor image to be used when the cursor is over the client area
-- of the specified window. The set cursor will only be visible when the cursor
-- mode of the window is GLFW_CURSOR_NORMAL.

-- On some platforms, the set cursor may not be visible unless the window also
-- has input focus.
setCursor :: Window -> Cursor -> IO ()
setCursor (Window wptr) (Cursor cptr) = c'glfwSetCursor wptr cptr

-- | Destroys a cursor previously created with `createCursor`. Any remaining
-- cursors will be destroyed by `terminate`.
destroyCursor :: Cursor -> IO ()
destroyCursor = c'glfwDestroyCursor . unCursor

-- | A callback that allows for drag and drop support.
type DropCallback = Window    -- ^ The window that received the event.
                  -> [String] -- ^ The file and/or directory path names
                  -> IO ()

-- | Sets the file drop callback of the specified window, which is called when
-- one or more dragged files are dropped on the window.
setDropCallback :: Window -> Maybe DropCallback -> IO ()
setDropCallback win = setWindowCallback
    mk'GLFWdropfun
    (\cb w c fs -> do
        let count = fromC c
        fps <- flip mapM [0..count-1] $ \i -> do
            let p = advancePtr fs i
            p' <- peek p
            peekCString p'
        schedule $ cb (fromC w) fps)
    (c'glfwSetDropCallback (toC win))
    storedDropFun
    win


--------------------------------------------------------------------------------
-- Vulkan-related functions
--------------------------------------------------------------------------------

-- | This function returns whether the Vulkan loader has been found.
--   This check is performed by `init`.
vulkanSupported :: IO Bool
vulkanSupported = (c'GLFW_TRUE ==) <$> c'glfwVulkanSupported

-- | Get required vulkan extensions;
--   Pointer memory is managed by GLFW, destroyed by `terminate` call.
--
--   The returned extension names are kept in `CString` type, because
--   they are expected to be consumed by vulkan device initialization functions.
getRequiredInstanceExtensions :: IO [CString]
getRequiredInstanceExtensions = alloca $ \countPtr -> do
    extsPtrPtr <- c'glfwGetRequiredInstanceExtensions countPtr
    count <- fromIntegral <$> peek countPtr
    peekArray count extsPtrPtr

-- | Returns the address of the specified Vulkan instance function.
getInstanceProcAddress :: Ptr vkInstance
                          -- ^ VkInstance.
                          --   Note, the returned function must be used
                          --   with the same instance or its child.
                       -> String
                          -- ^ Function name
                       -> IO (FunPtr vkProc)
getInstanceProcAddress i procName
  = withCString procName (c'glfwGetInstanceProcAddress i)

-- | Returns whether the specified queue family can present images.
getPhysicalDevicePresentationSupport ::
       Ptr vkInstance
       -- ^ VkInstance
    -> Ptr vkPhysicalDevice
       -- ^ VkPhysicalDevice
    -> Word32
       -- ^ Index of a queue family to query.
       --   This is an index in the array returned by
       --   @vkGetPhysicalDeviceQueueFamilyProperties@ function.
    -> IO Bool
getPhysicalDevicePresentationSupport inst dev i
  = (c'GLFW_TRUE ==) <$> c'glfwGetPhysicalDevicePresentationSupport inst dev i

-- | Creates a Vulkan surface for the specified window
createWindowSurface :: Enum vkResult
                    => Ptr vkInstance
                       -- ^ VkInstance
                    -> Window
                       -- ^ GLFWwindow *window
                    -> Ptr vkAllocationCallbacks
                       -- ^ const VkAllocationCallbacks *allocator
                    -> Ptr vkSurfaceKHR
                       -- ^ VkSurfaceKHR *surface
                    -> IO vkResult
createWindowSurface i win acs s
  = toEnum . fromIntegral
  <$> c'glfwCreateWindowSurface i (toC win) acs s
