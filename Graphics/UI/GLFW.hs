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
  , setScrollCallback,      ScrollCallback
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
  ) where

--------------------------------------------------------------------------------

import Prelude hiding (init)

import Control.Monad         (when)
import Data.IORef            (IORef, atomicModifyIORef', newIORef)
import Foreign.C.String      (peekCString, withCString)
import Foreign.C.Types       (CUInt, CUShort)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (advancePtr, allocaArray, peekArray, withArray)
import Foreign.Ptr           (FunPtr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable      (Storable(..))
import System.IO.Unsafe      (unsafePerformIO)

import Graphics.UI.GLFW.Internal.C           (C(..))
import Graphics.UI.GLFW.Internal.Instances.C ()
import Graphics.UI.GLFW.Types

import Bindings.GLFW

--------------------------------------------------------------------------------

-- We store FunPtrs from mk'GLFW*fun in these stored*Fun IORefs. Initialized
-- with unsafePerformIO, they are basically mutable global variables.

storedCharFun            :: IORef C'GLFWcharfun
storedCursorEnterFun     :: IORef C'GLFWcursorenterfun
storedCursorPosFun       :: IORef C'GLFWcursorposfun
storedErrorFun           :: IORef C'GLFWerrorfun
storedFramebufferSizeFun :: IORef C'GLFWframebuffersizefun
storedKeyFun             :: IORef C'GLFWkeyfun
storedMonitorFun         :: IORef C'GLFWmonitorfun
storedMouseButtonFun     :: IORef C'GLFWmousebuttonfun
storedScrollFun          :: IORef C'GLFWscrollfun
storedWindowCloseFun     :: IORef C'GLFWwindowclosefun
storedWindowFocusFun     :: IORef C'GLFWwindowfocusfun
storedWindowIconifyFun   :: IORef C'GLFWwindowiconifyfun
storedWindowPosFun       :: IORef C'GLFWwindowposfun
storedWindowRefreshFun   :: IORef C'GLFWwindowrefreshfun
storedWindowSizeFun      :: IORef C'GLFWwindowsizefun

storedCharFun            = unsafePerformIO $ newIORef nullFunPtr
storedCursorEnterFun     = unsafePerformIO $ newIORef nullFunPtr
storedCursorPosFun       = unsafePerformIO $ newIORef nullFunPtr
storedErrorFun           = unsafePerformIO $ newIORef nullFunPtr
storedFramebufferSizeFun = unsafePerformIO $ newIORef nullFunPtr
storedKeyFun             = unsafePerformIO $ newIORef nullFunPtr
storedMonitorFun         = unsafePerformIO $ newIORef nullFunPtr
storedMouseButtonFun     = unsafePerformIO $ newIORef nullFunPtr
storedScrollFun          = unsafePerformIO $ newIORef nullFunPtr
storedWindowCloseFun     = unsafePerformIO $ newIORef nullFunPtr
storedWindowFocusFun     = unsafePerformIO $ newIORef nullFunPtr
storedWindowIconifyFun   = unsafePerformIO $ newIORef nullFunPtr
storedWindowPosFun       = unsafePerformIO $ newIORef nullFunPtr
storedWindowRefreshFun   = unsafePerformIO $ newIORef nullFunPtr
storedWindowSizeFun      = unsafePerformIO $ newIORef nullFunPtr

-- These NOINLINE pragmas are due to use of unsafePerformIO.
-- See http://hackage.haskell.org/packages/archive/base/latest/doc/html/System-IO-Unsafe.html#v:unsafePerformIO .

{-# NOINLINE storedCharFun            #-}
{-# NOINLINE storedCursorEnterFun     #-}
{-# NOINLINE storedCursorPosFun       #-}
{-# NOINLINE storedErrorFun           #-}
{-# NOINLINE storedFramebufferSizeFun #-}
{-# NOINLINE storedKeyFun             #-}
{-# NOINLINE storedMonitorFun         #-}
{-# NOINLINE storedMouseButtonFun     #-}
{-# NOINLINE storedScrollFun          #-}
{-# NOINLINE storedWindowCloseFun     #-}
{-# NOINLINE storedWindowFocusFun     #-}
{-# NOINLINE storedWindowIconifyFun   #-}
{-# NOINLINE storedWindowPosFun       #-}
{-# NOINLINE storedWindowRefreshFun   #-}
{-# NOINLINE storedWindowSizeFun      #-}

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
    prev <- atomicModifyIORef' ior (\cur -> (new, cur))
    -- Free the old FunPtr if necessary.
    when (prev /= nullFunPtr) $ freeHaskellFunPtr prev

--------------------------------------------------------------------------------

type ErrorCallback           = Error -> String                                           -> IO ()
type WindowPosCallback       = Window -> Int -> Int                                      -> IO ()
type WindowSizeCallback      = Window -> Int -> Int                                      -> IO ()
type WindowCloseCallback     = Window                                                    -> IO ()
type WindowRefreshCallback   = Window                                                    -> IO ()
type WindowFocusCallback     = Window -> FocusState                                      -> IO ()
type WindowIconifyCallback   = Window -> IconifyState                                    -> IO ()
type FramebufferSizeCallback = Window -> Int -> Int                                      -> IO ()
type MouseButtonCallback     = Window -> MouseButton -> MouseButtonState -> ModifierKeys -> IO ()
type CursorPosCallback       = Window -> Double -> Double                                -> IO ()
type CursorEnterCallback     = Window -> CursorState                                     -> IO ()
type ScrollCallback          = Window -> Double -> Double                                -> IO ()
type KeyCallback             = Window -> Key -> Int -> KeyState -> ModifierKeys          -> IO ()
type CharCallback            = Window -> Char                                            -> IO ()
type MonitorCallback         = Monitor -> MonitorState                                   -> IO ()

--------------------------------------------------------------------------------
-- Error handling

setErrorCallback :: Maybe ErrorCallback -> IO ()
setErrorCallback = setCallback
    mk'GLFWerrorfun
    (\cb a0 a1 -> do
        s <- peekCString a1
        cb (fromC a0) s)
    c'glfwSetErrorCallback
    storedErrorFun

--------------------------------------------------------------------------------
-- Initialization and version information

init :: IO Bool
init =
    fromC `fmap` c'glfwInit

terminate :: IO ()
terminate = do
    c'glfwTerminate
    -- Free all stored FunPtrs.
    storeCallback storedCharFun            nullFunPtr
    storeCallback storedCursorEnterFun     nullFunPtr
    storeCallback storedCursorPosFun       nullFunPtr
    storeCallback storedErrorFun           nullFunPtr
    storeCallback storedFramebufferSizeFun nullFunPtr
    storeCallback storedKeyFun             nullFunPtr
    storeCallback storedMonitorFun         nullFunPtr
    storeCallback storedMouseButtonFun     nullFunPtr
    storeCallback storedScrollFun          nullFunPtr
    storeCallback storedWindowCloseFun     nullFunPtr
    storeCallback storedWindowFocusFun     nullFunPtr
    storeCallback storedWindowIconifyFun   nullFunPtr
    storeCallback storedWindowPosFun       nullFunPtr
    storeCallback storedWindowRefreshFun   nullFunPtr
    storeCallback storedWindowSizeFun      nullFunPtr

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

getVersionString :: IO (Maybe String)
getVersionString = do
    p'vs <- c'glfwGetVersionString
    if p'vs /= nullPtr
      then Just `fmap` peekCString p'vs
      else return Nothing

--------------------------------------------------------------------------------
-- Monitor handling

getMonitors :: IO (Maybe [Monitor])
getMonitors =
    alloca $ \p'n -> do
        p'mon <- c'glfwGetMonitors p'n
        n <- fromC `fmap` peek p'n
        if p'mon == nullPtr || n <= 0
          then return Nothing
          else (Just . map fromC) `fmap` peekArray n p'mon

getPrimaryMonitor :: IO (Maybe Monitor)
getPrimaryMonitor = do
    p'mon <- c'glfwGetPrimaryMonitor
    return $
      if p'mon == nullPtr
        then Nothing
        else Just $ fromC p'mon

getMonitorPos :: Monitor -> IO (Int, Int)
getMonitorPos mon =
    allocaArray 2 $ \p -> do
        let p'x = p
            p'y = p `advancePtr` 1
        c'glfwGetMonitorPos (toC mon) p'x p'y
        x <- fromC `fmap` peek p'x
        y <- fromC `fmap` peek p'y
        return (x, y)

getMonitorPhysicalSize :: Monitor -> IO (Int, Int)
getMonitorPhysicalSize mon =
    allocaArray 2 $ \p -> do
        let p'w = p
            p'h = p `advancePtr` 1
        c'glfwGetMonitorPhysicalSize (toC mon) p'w p'h
        w <- fromC `fmap` peek p'w
        h <- fromC `fmap` peek p'h
        return (w, h)

getMonitorName :: Monitor -> IO (Maybe String)
getMonitorName mon = do
    p'name <- c'glfwGetMonitorName (toC mon)
    if p'name == nullPtr
      then return Nothing
      else Just `fmap` peekCString p'name

setMonitorCallback :: Maybe MonitorCallback -> IO ()
setMonitorCallback = setCallback
    mk'GLFWmonitorfun
    (\cb a0 a1 -> cb (fromC a0) (fromC a1))
    c'glfwSetMonitorCallback
    storedMonitorFun

getVideoModes :: Monitor -> IO (Maybe [VideoMode])
getVideoModes mon =
    alloca $ \p'n -> do
        p'vms <- c'glfwGetVideoModes (toC mon) p'n
        n <- fromC `fmap` peek p'n
        if p'vms == nullPtr || n <= 0
          then return Nothing
          else (Just . map fromC) `fmap` peekArray n p'vms

getVideoMode :: Monitor -> IO (Maybe VideoMode)
getVideoMode mon = do
    p'vm <- c'glfwGetVideoMode (toC mon)
    if p'vm == nullPtr
      then return Nothing
      else (Just . fromC) `fmap` peek p'vm

setGamma :: Monitor -> Double -> IO ()
setGamma mon e =
    c'glfwSetGamma (toC mon) (toC e)

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

defaultWindowHints :: IO ()
defaultWindowHints =
    c'glfwDefaultWindowHints

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

createWindow :: Int -> Int -> String -> Maybe Monitor -> Maybe Window -> IO (Maybe Window)
createWindow w h title mmon mwin =
    withCString title $ \ptitle -> do
        p'win <- c'glfwCreateWindow
          (toC w)
          (toC h)
          ptitle
          (maybe nullPtr toC mmon)
          (maybe nullPtr toC mwin)
        return $ if p'win == nullPtr
          then Nothing
          else Just $ fromC p'win

destroyWindow :: Window -> IO ()
destroyWindow =
    c'glfwDestroyWindow . toC

windowShouldClose :: Window -> IO Bool
windowShouldClose win =
    fromC `fmap` c'glfwWindowShouldClose (toC win)

setWindowShouldClose :: Window -> Bool -> IO ()
setWindowShouldClose win b =
    c'glfwSetWindowShouldClose (toC win) (toC b)

setWindowTitle :: Window -> String -> IO ()
setWindowTitle win title =
    withCString title $ c'glfwSetWindowTitle (toC win)

getWindowPos :: Window -> IO (Int, Int)
getWindowPos win =
    allocaArray 2 $ \p -> do
        let p'x = p
            p'y = p `advancePtr` 1
        c'glfwGetWindowPos (toC win) p'x p'y
        x <- fromC `fmap` peek p'x
        y <- fromC `fmap` peek p'y
        return (x, y)

setWindowPos :: Window -> Int -> Int -> IO ()
setWindowPos win x y =
    c'glfwSetWindowPos (toC win) (toC x) (toC y)

getWindowSize :: Window -> IO (Int, Int)
getWindowSize win =
    allocaArray 2 $ \p -> do
        let p'w = p
            p'h = p `advancePtr` 1
        c'glfwGetWindowSize (toC win) p'w p'h
        w <- fromC `fmap` peek p'w
        h <- fromC `fmap` peek p'h
        return (w, h)

setWindowSize :: Window -> Int -> Int -> IO ()
setWindowSize win w h =
    c'glfwSetWindowSize (toC win) (toC w) (toC h)

getFramebufferSize :: Window -> IO (Int, Int)
getFramebufferSize win =
    allocaArray 2 $ \p -> do
        let p'w = p
            p'h = p `advancePtr` 1
        c'glfwGetFramebufferSize (toC win) p'w p'h
        w <- fromC `fmap` peek p'w
        h <- fromC `fmap` peek p'h
        return (w, h)

iconifyWindow :: Window -> IO ()
iconifyWindow =
    c'glfwIconifyWindow . toC

restoreWindow :: Window -> IO ()
restoreWindow =
    c'glfwRestoreWindow . toC

showWindow :: Window -> IO ()
showWindow =
    c'glfwShowWindow . toC

hideWindow :: Window -> IO ()
hideWindow =
    c'glfwHideWindow . toC

getWindowMonitor :: Window -> IO (Maybe Monitor)
getWindowMonitor win = do
    p'mon <- c'glfwGetWindowMonitor (toC win)
    return $ if p'mon == nullPtr
      then Nothing
      else Just $ fromC p'mon

setCursorPos :: Window -> Double -> Double -> IO ()
setCursorPos win x y =
    c'glfwSetCursorPos (toC win) (toC x) (toC y)

-- start of functions related to c'glfwGetWindowAttrib

getWindowFocused :: Window -> IO FocusState
getWindowFocused win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_FOCUSED

getWindowIconified :: Window -> IO IconifyState
getWindowIconified win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_ICONIFIED

getWindowResizable :: Window -> IO Bool
getWindowResizable win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_RESIZABLE

getWindowDecorated :: Window -> IO Bool
getWindowDecorated win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_DECORATED

getWindowVisible :: Window -> IO Bool
getWindowVisible win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_VISIBLE

getWindowClientAPI :: Window -> IO ClientAPI
getWindowClientAPI win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_CLIENT_API

getWindowContextVersionMajor :: Window -> IO Int
getWindowContextVersionMajor win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_CONTEXT_VERSION_MAJOR

getWindowContextVersionMinor :: Window -> IO Int
getWindowContextVersionMinor win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_CONTEXT_VERSION_MINOR

getWindowContextVersionRevision :: Window -> IO Int
getWindowContextVersionRevision win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_CONTEXT_REVISION

getWindowContextRobustness :: Window -> IO ContextRobustness
getWindowContextRobustness win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_CONTEXT_ROBUSTNESS

getWindowOpenGLForwardCompat :: Window -> IO Bool
getWindowOpenGLForwardCompat win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_OPENGL_FORWARD_COMPAT

getWindowOpenGLDebugContext :: Window -> IO Bool
getWindowOpenGLDebugContext win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_OPENGL_DEBUG_CONTEXT

getWindowOpenGLProfile :: Window -> IO OpenGLProfile
getWindowOpenGLProfile win =
    fromC `fmap` c'glfwGetWindowAttrib (toC win) c'GLFW_OPENGL_PROFILE

-- end of functions related to c'glfwGetWindowAttrib

setWindowPosCallback :: Window -> Maybe WindowPosCallback -> IO ()
setWindowPosCallback win = setCallback
    mk'GLFWwindowposfun
    (\cb a0 a1 a2 ->
      cb (fromC a0) (fromC a1) (fromC a2))
    (c'glfwSetWindowPosCallback (toC win))
    storedWindowPosFun

setWindowSizeCallback :: Window -> Maybe WindowSizeCallback -> IO ()
setWindowSizeCallback win = setCallback
    mk'GLFWwindowsizefun
    (\cb a0 a1 a2 ->
      cb (fromC a0) (fromC a1) (fromC a2))
    (c'glfwSetWindowSizeCallback (toC win))
    storedWindowSizeFun

setWindowCloseCallback :: Window -> Maybe WindowCloseCallback -> IO ()
setWindowCloseCallback win = setCallback
    mk'GLFWwindowclosefun
    (. fromC)
    (c'glfwSetWindowCloseCallback (toC win))
    storedWindowCloseFun

setWindowRefreshCallback :: Window -> Maybe WindowRefreshCallback -> IO ()
setWindowRefreshCallback win = setCallback
    mk'GLFWwindowrefreshfun
    (. fromC)
    (c'glfwSetWindowRefreshCallback (toC win))
    storedWindowRefreshFun

setWindowFocusCallback :: Window -> Maybe WindowFocusCallback -> IO ()
setWindowFocusCallback win = setCallback
    mk'GLFWwindowfocusfun
    (\cb a0 a1 -> cb (fromC a0) (fromC a1))
    (c'glfwSetWindowFocusCallback (toC win))
    storedWindowFocusFun

setWindowIconifyCallback :: Window -> Maybe WindowIconifyCallback -> IO ()
setWindowIconifyCallback win = setCallback
    mk'GLFWwindowiconifyfun
    (\cb a0 a1 -> cb (fromC a0) (fromC a1))
    (c'glfwSetWindowIconifyCallback (toC win))
    storedWindowIconifyFun

setFramebufferSizeCallback :: Window -> Maybe FramebufferSizeCallback -> IO ()
setFramebufferSizeCallback win = setCallback
    mk'GLFWframebuffersizefun
    (\cb a0 a1 a2 -> cb (fromC a0) (fromC a1) (fromC a2))
    (c'glfwSetFramebufferSizeCallback (toC win))
    storedFramebufferSizeFun

pollEvents :: IO ()
pollEvents = c'glfwPollEvents

waitEvents :: IO ()
waitEvents = c'glfwWaitEvents

--------------------------------------------------------------------------------
-- Input handling

-- start of glfw{GS}etInputMode-related functions

getCursorInputMode :: Window -> IO CursorInputMode
getCursorInputMode win =
    fromC `fmap` c'glfwGetInputMode (toC win) c'GLFW_CURSOR

setCursorInputMode :: Window -> CursorInputMode -> IO ()
setCursorInputMode win c =
    c'glfwSetInputMode (toC win) c'GLFW_CURSOR (toC c)

getStickyKeysInputMode :: Window -> IO StickyKeysInputMode
getStickyKeysInputMode win =
    fromC `fmap` c'glfwGetInputMode (toC win) c'GLFW_STICKY_KEYS

setStickyKeysInputMode :: Window -> StickyKeysInputMode -> IO ()
setStickyKeysInputMode win sk =
    c'glfwSetInputMode (toC win) c'GLFW_STICKY_KEYS (toC sk)

getStickyMouseButtonsInputMode :: Window -> IO StickyMouseButtonsInputMode
getStickyMouseButtonsInputMode win =
    fromC `fmap` c'glfwGetInputMode (toC win) c'GLFW_STICKY_MOUSE_BUTTONS

setStickyMouseButtonsInputMode :: Window -> StickyMouseButtonsInputMode -> IO ()
setStickyMouseButtonsInputMode win smb =
    c'glfwSetInputMode (toC win) c'GLFW_STICKY_MOUSE_BUTTONS (toC smb)

-- end of glfw{GS}etInputMode-related functions

getKey :: Window -> Key -> IO KeyState
getKey win k =
    fromC `fmap` c'glfwGetKey (toC win) (toC k)

getMouseButton :: Window -> MouseButton -> IO MouseButtonState
getMouseButton win b =
    fromC `fmap` c'glfwGetMouseButton (toC win) (toC b)

getCursorPos :: Window -> IO (Double, Double)
getCursorPos win =
    allocaArray 2 $ \p -> do
        let p'x = p
            p'y = p `advancePtr` 1
        c'glfwGetCursorPos (toC win) p'x p'y
        x <- fromC `fmap` peek p'x
        y <- fromC `fmap` peek p'y
        return (x, y)

setKeyCallback :: Window -> Maybe KeyCallback -> IO ()
setKeyCallback win = setCallback
    mk'GLFWkeyfun
    (\cb a0 a1 a2 a3 a4 ->
      cb (fromC a0) (fromC a1) (fromC a2) (fromC a3) (fromC a4))
    (c'glfwSetKeyCallback (toC win))
    storedKeyFun

setCharCallback :: Window -> Maybe CharCallback -> IO ()
setCharCallback win = setCallback
    mk'GLFWcharfun
    (\cb a0 a1 -> cb (fromC a0) (fromC a1))
    (c'glfwSetCharCallback (toC win))
    storedCharFun

setMouseButtonCallback :: Window -> Maybe MouseButtonCallback -> IO ()
setMouseButtonCallback win = setCallback
    mk'GLFWmousebuttonfun
    (\cb a0 a1 a2 a3 -> cb (fromC a0) (fromC a1) (fromC a2) (fromC a3))
    (c'glfwSetMouseButtonCallback (toC win))
    storedMouseButtonFun

setCursorPosCallback :: Window -> Maybe CursorPosCallback -> IO ()
setCursorPosCallback win = setCallback
    mk'GLFWcursorposfun
    (\cb a0 a1 a2 -> cb (fromC a0) (fromC a1) (fromC a2))
    (c'glfwSetCursorPosCallback (toC win))
    storedCursorPosFun

setCursorEnterCallback :: Window -> Maybe CursorEnterCallback -> IO ()
setCursorEnterCallback win = setCallback
    mk'GLFWcursorenterfun
    (\cb a0 a1 -> cb (fromC a0) (fromC a1))
    (c'glfwSetCursorEnterCallback (toC win))
    storedCursorEnterFun

setScrollCallback :: Window -> Maybe ScrollCallback -> IO ()
setScrollCallback win = setCallback
    mk'GLFWscrollfun
    (\cb a0 a1 a2 -> cb (fromC a0) (fromC a1) (fromC a2))
    (c'glfwSetScrollCallback (toC win))
    storedScrollFun

joystickPresent :: Joystick -> IO Bool
joystickPresent js =
    fromC `fmap` c'glfwJoystickPresent (toC js)

getJoystickAxes :: Joystick -> IO (Maybe [Double])
getJoystickAxes js =
    alloca $ \p'n -> do
        p'axes <- c'glfwGetJoystickAxes (toC js) p'n
        n <- fromC `fmap` peek p'n
        if p'axes == nullPtr || n <= 0
          then return Nothing
          else (Just . map fromC) `fmap` peekArray n p'axes

getJoystickButtons :: Joystick -> IO (Maybe [JoystickButtonState])
getJoystickButtons js =
    alloca $ \p'n -> do
        p'buttons <- c'glfwGetJoystickButtons (toC js) p'n
        n <- fromC `fmap` peek p'n
        if p'buttons == nullPtr || n <= 0
          then return Nothing
          else (Just . map fromC) `fmap` peekArray n p'buttons

getJoystickName :: Joystick -> IO (Maybe String)
getJoystickName js = do
    p'name <- c'glfwGetJoystickName (toC js)
    if p'name == nullPtr
      then return Nothing
      else Just `fmap` peekCString p'name

--------------------------------------------------------------------------------
-- Time

getTime :: IO (Maybe Double)
getTime = do
    t <- fromC `fmap` c'glfwGetTime
    return $ if t == 0
      then Nothing
      else Just t

setTime :: Double -> IO ()
setTime =
    c'glfwSetTime . toC

--------------------------------------------------------------------------------
-- Context

makeContextCurrent :: Maybe Window -> IO ()
makeContextCurrent =
    c'glfwMakeContextCurrent . maybe nullPtr toC

getCurrentContext :: IO (Maybe Window)
getCurrentContext = do
    p'win <- c'glfwGetCurrentContext
    return $ if p'win == nullPtr
      then Nothing
      else Just $ fromC p'win

swapBuffers :: Window -> IO ()
swapBuffers =
    c'glfwSwapBuffers . toC

swapInterval :: Int -> IO ()
swapInterval =
    c'glfwSwapInterval . toC

extensionSupported :: String -> IO Bool
extensionSupported ext =
    withCString ext $ \p'ext ->
      fromC `fmap` c'glfwExtensionSupported p'ext

--------------------------------------------------------------------------------
-- Clipboard

setClipboardString :: Window -> String -> IO ()
setClipboardString win s =
    withCString s (c'glfwSetClipboardString (toC win))

getClipboardString :: Window -> IO (Maybe String)
getClipboardString win = do
    p's <- c'glfwGetClipboardString (toC win)
    if p's == nullPtr
      then return Nothing
      else Just `fmap` peekCString p's
