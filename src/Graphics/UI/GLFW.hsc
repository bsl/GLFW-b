{-# LANGUAGE    ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

#include <GLFW/glfw3.h>

{-
When we set a callback, do we want the previous one?
In 'terminate', should we clear all stored callbacks?
-}

module Graphics.UI.GLFW
  ( initialize
  , terminate
  , getVersion
  , getVersionString
  , setErrorCallback
  , getMonitors
  , getPrimaryMonitor
  , getMonitorPosition
  , getMonitorPhysicalSize
  , getMonitorName
  , setMonitorCallback
  , getVideoModes
  , getVideoMode
  , setGamma
  , setGammaRamp
  , getGammaRamp
  , setDefaultWindowHints
  , defaultWindowHints
  , setWindowHints
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
  , getWindowAttrib
  , setWindowPosCallback
  , setWindowSizeCallback
  , setWindowCloseCallback
  , setWindowRefreshCallback
  , setWindowFocusCallback
  , setWindowIconifyCallback
  , setFramebufferSizeCallback
  , pollEvents
  , waitEvents
  -- {s,g}etInputMode is covered by next 6 {s,g}et*InputMode
  , getCursorInputMode
  , setCursorInputMode
  , getKeysInputMode
  , setKeysInputMode
  , getMouseButtonsInputMode
  , setMouseButtonsInputMode
  , getKey
  , getMouseButton
  , getCursorPos
  , setCursorPos
  , setKeyCallback
  , setCharCallback
  , setMouseButtonCallback
  , setCursorPosCallback
  , setCursorEnterCallback
  , setScrollCallback
  , joystickPresent
  , getJoystickAxes
  , getJoystickButtons
  , getJoystickName
  , setClipboardString
  , getClipboardString
  , getTime
  , setTime
  , makeContextCurrent
  , getCurrentContext
  , swapBuffers
  , swapInterval
  , extensionSupported
  -- getProcAddress
  , Version(..)
  , Joystick(..)
  , GammaRamp(..)
  , Key(..)
  , KeyAction(..)
  , Window
  , Error(..)
  , FocusAction(..)
  , IconifyAction(..)
  , MouseButton(..)
  , MouseButtonAction(..)
  , ModifierKeys(..)
  , MonitorAction(..)
  , CursorAction(..)
  ) where

--------------------------------------------------------------------------------

import Control.Monad         (when)
import Data.IORef            (IORef, atomicModifyIORef', newIORef)
import Foreign.C.String      (peekCString, withCString)
import Foreign.C.Types       (CChar(..), CDouble(..), CFloat(..), CInt(..), CUShort(..), CUChar(..), CUInt(..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (advancePtr, allocaArray, withArray, peekArray)
import Foreign.Ptr           (FunPtr, Ptr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable      (Storable(..))
import System.IO.Unsafe      (unsafePerformIO)

import Graphics.UI.GLFW.Internal.C           (C(..))
import Graphics.UI.GLFW.Internal.Instances.C ()
import Graphics.UI.GLFW.Types

--------------------------------------------------------------------------------

-- TODO: which of these can be marked "unsafe"?
foreign import ccall glfwInit                       :: IO CInt
foreign import ccall glfwTerminate                  :: IO ()
foreign import ccall glfwGetVersion                 :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall glfwGetVersionString           :: IO (Ptr CChar)
foreign import ccall glfwSetErrorCallback           :: FunPtr GlfwErrorCallback -> IO (FunPtr GlfwErrorCallback)
foreign import ccall glfwGetMonitors                :: Ptr CInt -> IO (Ptr (Ptr GlfwMonitor))
foreign import ccall glfwGetPrimaryMonitor          :: IO (Ptr GlfwMonitor)
foreign import ccall glfwGetMonitorPos              :: Ptr GlfwMonitor -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall glfwGetMonitorPhysicalSize     :: Ptr GlfwMonitor -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall glfwGetMonitorName             :: Ptr GlfwMonitor -> IO (Ptr CChar)
foreign import ccall glfwSetMonitorCallback         :: FunPtr GlfwMonitorCallback -> IO (FunPtr GlfwMonitorCallback)
foreign import ccall glfwGetVideoModes              :: Ptr GlfwMonitor -> Ptr CInt -> IO (Ptr GlfwVideoMode)
foreign import ccall glfwGetVideoMode               :: Ptr GlfwMonitor -> IO (Ptr GlfwVideoMode)
foreign import ccall glfwSetGamma                   :: Ptr GlfwMonitor -> CFloat -> IO ()
foreign import ccall glfwSetGammaRamp               :: Ptr GlfwMonitor -> Ptr GlfwGammaRamp -> IO ()
foreign import ccall glfwGetGammaRamp               :: Ptr GlfwMonitor -> IO (Ptr GlfwGammaRamp)
foreign import ccall glfwDefaultWindowHints         :: IO ()
foreign import ccall glfwWindowHint                 :: CInt -> CInt -> IO ()
foreign import ccall glfwCreateWindow               :: CInt -> CInt -> Ptr CChar -> Ptr GlfwMonitor -> Ptr GlfwWindow -> IO (Ptr GlfwWindow)
foreign import ccall glfwDestroyWindow              :: Ptr GlfwWindow -> IO ()
foreign import ccall glfwWindowShouldClose          :: Ptr GlfwWindow -> IO CInt
foreign import ccall glfwSetWindowShouldClose       :: Ptr GlfwWindow -> CInt -> IO ()
foreign import ccall glfwSetWindowTitle             :: Ptr GlfwWindow -> Ptr CChar -> IO ()
foreign import ccall glfwGetWindowPos               :: Ptr GlfwWindow -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall glfwSetWindowPos               :: Ptr GlfwWindow -> CInt -> CInt -> IO ()
foreign import ccall glfwGetWindowSize              :: Ptr GlfwWindow -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall glfwSetWindowSize              :: Ptr GlfwWindow -> CInt -> CInt -> IO ()
foreign import ccall glfwGetFramebufferSize         :: Ptr GlfwWindow -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall glfwIconifyWindow              :: Ptr GlfwWindow -> IO ()
foreign import ccall glfwRestoreWindow              :: Ptr GlfwWindow -> IO ()
foreign import ccall glfwShowWindow                 :: Ptr GlfwWindow -> IO ()
foreign import ccall glfwHideWindow                 :: Ptr GlfwWindow -> IO ()
foreign import ccall glfwGetWindowMonitor           :: Ptr GlfwWindow -> IO (Ptr GlfwMonitor)
foreign import ccall glfwGetWindowAttrib            :: Ptr GlfwWindow -> CInt -> IO CInt
foreign import ccall glfwSetWindowPosCallback       :: Ptr GlfwWindow -> FunPtr GlfwWindowPosCallback       -> IO (FunPtr GlfwWindowPosCallback)
foreign import ccall glfwSetWindowSizeCallback      :: Ptr GlfwWindow -> FunPtr GlfwWindowSizeCallback      -> IO (FunPtr GlfwWindowSizeCallback)
foreign import ccall glfwSetWindowCloseCallback     :: Ptr GlfwWindow -> FunPtr GlfwWindowCloseCallback     -> IO (FunPtr GlfwWindowCloseCallback)
foreign import ccall glfwSetWindowRefreshCallback   :: Ptr GlfwWindow -> FunPtr GlfwWindowRefreshCallback   -> IO (FunPtr GlfwWindowRefreshCallback)
foreign import ccall glfwSetWindowFocusCallback     :: Ptr GlfwWindow -> FunPtr GlfwWindowFocusCallback     -> IO (FunPtr GlfwWindowFocusCallback)
foreign import ccall glfwSetWindowIconifyCallback   :: Ptr GlfwWindow -> FunPtr GlfwWindowIconifyCallback   -> IO (FunPtr GlfwWindowIconifyCallback)
foreign import ccall glfwSetFramebufferSizeCallback :: Ptr GlfwWindow -> FunPtr GlfwFramebufferSizeCallback -> IO (FunPtr GlfwFramebufferSizeCallback)
foreign import ccall glfwPollEvents                 :: IO ()
foreign import ccall glfwWaitEvents                 :: IO ()
foreign import ccall glfwGetInputMode               :: Ptr GlfwWindow -> CInt -> IO CInt
foreign import ccall glfwSetInputMode               :: Ptr GlfwWindow -> CInt -> CInt -> IO ()
foreign import ccall glfwGetKey                     :: Ptr GlfwWindow -> CInt -> IO CInt
foreign import ccall glfwGetMouseButton             :: Ptr GlfwWindow -> CInt -> IO CInt
foreign import ccall glfwGetCursorPos               :: Ptr GlfwWindow -> Ptr CDouble -> Ptr CDouble -> IO ()
foreign import ccall glfwSetCursorPos               :: Ptr GlfwWindow -> CDouble -> CDouble -> IO ()
foreign import ccall glfwSetKeyCallback             :: Ptr GlfwWindow -> FunPtr GlfwKeyCallback -> IO (FunPtr GlfwKeyCallback)
foreign import ccall glfwSetCharCallback            :: Ptr GlfwWindow -> FunPtr GlfwCharCallback -> IO (FunPtr GlfwCharCallback)
foreign import ccall glfwSetMouseButtonCallback     :: Ptr GlfwWindow -> FunPtr GlfwMouseButtonCallback -> IO (FunPtr GlfwMouseButtonCallback)
foreign import ccall glfwSetCursorPosCallback       :: Ptr GlfwWindow -> FunPtr GlfwCursorPosCallback -> IO (FunPtr GlfwCursorPosCallback)
foreign import ccall glfwSetCursorEnterCallback     :: Ptr GlfwWindow -> FunPtr GlfwCursorEnterCallback -> IO (FunPtr GlfwCursorEnterCallback)
foreign import ccall glfwSetScrollCallback          :: Ptr GlfwWindow -> FunPtr GlfwScrollCallback -> IO (FunPtr GlfwScrollCallback)
foreign import ccall glfwJoystickPresent            :: CInt -> IO CInt
foreign import ccall glfwGetJoystickAxes            :: CInt -> Ptr CInt -> IO (Ptr CFloat)
foreign import ccall glfwGetJoystickButtons         :: CInt -> Ptr CInt -> IO (Ptr CUChar)
foreign import ccall glfwGetJoystickName            :: CInt -> IO (Ptr CChar)
foreign import ccall glfwSetClipboardString         :: Ptr GlfwWindow -> Ptr CChar -> IO ()
foreign import ccall glfwGetClipboardString         :: Ptr GlfwWindow -> IO (Ptr CChar)
foreign import ccall glfwGetTime                    :: IO CDouble
foreign import ccall glfwSetTime                    :: CDouble -> IO ()
foreign import ccall glfwMakeContextCurrent         :: Ptr GlfwWindow -> IO ()
foreign import ccall glfwGetCurrentContext          :: IO (Ptr GlfwWindow)
foreign import ccall glfwSwapBuffers                :: Ptr GlfwWindow -> IO ()
foreign import ccall glfwSwapInterval               :: CInt -> IO ()
foreign import ccall glfwExtensionSupported         :: Ptr CChar -> IO CInt

-- glfwSetWindowUserPointer
-- glfwGetWindowUserPointer
-- glfwGetProcAddress

type GlfwErrorCallback           = CInt -> Ptr CChar                              -> IO ()
type GlfwMonitorCallback         = Ptr GlfwMonitor -> CInt                        -> IO ()
type GlfwCharCallback            = Ptr GlfwWindow -> CInt                         -> IO ()
type GlfwCursorEnterCallback     = Ptr GlfwWindow -> CInt                         -> IO ()
type GlfwCursorPosCallback       = Ptr GlfwWindow -> CDouble -> CDouble           -> IO ()
type GlfwFramebufferSizeCallback = Ptr GlfwWindow -> CInt -> CInt                 -> IO ()
type GlfwKeyCallback             = Ptr GlfwWindow -> CInt -> CInt -> CInt -> CInt -> IO ()
type GlfwMouseButtonCallback     = Ptr GlfwWindow -> CInt -> CInt -> CInt         -> IO ()
type GlfwScrollCallback          = Ptr GlfwWindow -> CDouble -> CDouble           -> IO ()
type GlfwWindowCloseCallback     = Ptr GlfwWindow                                 -> IO ()
type GlfwWindowFocusCallback     = Ptr GlfwWindow -> CInt                         -> IO ()
type GlfwWindowIconifyCallback   = Ptr GlfwWindow -> CInt                         -> IO ()
type GlfwWindowPosCallback       = Ptr GlfwWindow -> CInt -> CInt                 -> IO ()
type GlfwWindowRefreshCallback   = Ptr GlfwWindow                                 -> IO ()
type GlfwWindowSizeCallback      = Ptr GlfwWindow -> CInt -> CInt                 -> IO ()

foreign import ccall "wrapper" wrapGlfwErrorCallback           :: GlfwErrorCallback           -> IO (FunPtr GlfwErrorCallback           )
foreign import ccall "wrapper" wrapGlfwMonitorCallback         :: GlfwMonitorCallback         -> IO (FunPtr GlfwMonitorCallback         )
foreign import ccall "wrapper" wrapGlfwCharCallback            :: GlfwCharCallback            -> IO (FunPtr GlfwCharCallback            )
foreign import ccall "wrapper" wrapGlfwCursorEnterCallback     :: GlfwCursorEnterCallback     -> IO (FunPtr GlfwCursorEnterCallback     )
foreign import ccall "wrapper" wrapGlfwCursorPosCallback       :: GlfwCursorPosCallback       -> IO (FunPtr GlfwCursorPosCallback       )
foreign import ccall "wrapper" wrapGlfwFramebufferSizeCallback :: GlfwFramebufferSizeCallback -> IO (FunPtr GlfwFramebufferSizeCallback )
foreign import ccall "wrapper" wrapGlfwKeyCallback             :: GlfwKeyCallback             -> IO (FunPtr GlfwKeyCallback             )
foreign import ccall "wrapper" wrapGlfwMouseButtonCallback     :: GlfwMouseButtonCallback     -> IO (FunPtr GlfwMouseButtonCallback     )
foreign import ccall "wrapper" wrapGlfwScrollCallback          :: GlfwScrollCallback          -> IO (FunPtr GlfwScrollCallback          )
foreign import ccall "wrapper" wrapGlfwWindowCloseCallback     :: GlfwWindowCloseCallback     -> IO (FunPtr GlfwWindowCloseCallback     )
foreign import ccall "wrapper" wrapGlfwWindowFocusCallback     :: GlfwWindowFocusCallback     -> IO (FunPtr GlfwWindowFocusCallback     )
foreign import ccall "wrapper" wrapGlfwWindowIconifyCallback   :: GlfwWindowIconifyCallback   -> IO (FunPtr GlfwWindowIconifyCallback   )
foreign import ccall "wrapper" wrapGlfwWindowPosCallback       :: GlfwWindowPosCallback       -> IO (FunPtr GlfwWindowPosCallback       )
foreign import ccall "wrapper" wrapGlfwWindowRefreshCallback   :: GlfwWindowRefreshCallback   -> IO (FunPtr GlfwWindowRefreshCallback   )
foreign import ccall "wrapper" wrapGlfwWindowSizeCallback      :: GlfwWindowSizeCallback      -> IO (FunPtr GlfwWindowSizeCallback      )

--------------------------------------------------------------------------------

errorCallback           :: IORef (FunPtr GlfwErrorCallback)
{-# NOINLINE errorCallback           #-}
windowPosCallback       :: IORef (FunPtr GlfwWindowPosCallback)
{-# NOINLINE windowPosCallback       #-}
windowSizeCallback      :: IORef (FunPtr GlfwWindowSizeCallback)
{-# NOINLINE windowSizeCallback      #-}
windowCloseCallback     :: IORef (FunPtr GlfwWindowCloseCallback)
{-# NOINLINE windowCloseCallback     #-}
windowRefreshCallback   :: IORef (FunPtr GlfwWindowRefreshCallback)
{-# NOINLINE windowRefreshCallback   #-}
windowFocusCallback     :: IORef (FunPtr GlfwWindowFocusCallback)
{-# NOINLINE windowFocusCallback     #-}
windowIconifyCallback   :: IORef (FunPtr GlfwWindowIconifyCallback)
{-# NOINLINE windowIconifyCallback   #-}
framebufferSizeCallback :: IORef (FunPtr GlfwFramebufferSizeCallback)
{-# NOINLINE framebufferSizeCallback #-}
mouseButtonCallback     :: IORef (FunPtr GlfwMouseButtonCallback)
{-# NOINLINE mouseButtonCallback     #-}
cursorPosCallback       :: IORef (FunPtr GlfwCursorPosCallback)
{-# NOINLINE cursorPosCallback       #-}
cursorEnterCallback     :: IORef (FunPtr GlfwCursorEnterCallback)
{-# NOINLINE cursorEnterCallback     #-}
scrollCallback          :: IORef (FunPtr GlfwScrollCallback)
{-# NOINLINE scrollCallback          #-}
keyCallback             :: IORef (FunPtr GlfwKeyCallback)
{-# NOINLINE keyCallback             #-}
charCallback            :: IORef (FunPtr GlfwCharCallback)
{-# NOINLINE charCallback            #-}
monitorCallback         :: IORef (FunPtr GlfwMonitorCallback)
{-# NOINLINE monitorCallback         #-}

errorCallback           = unsafePerformIO $ newIORef nullFunPtr
windowPosCallback       = unsafePerformIO $ newIORef nullFunPtr
windowSizeCallback      = unsafePerformIO $ newIORef nullFunPtr
windowCloseCallback     = unsafePerformIO $ newIORef nullFunPtr
windowRefreshCallback   = unsafePerformIO $ newIORef nullFunPtr
windowFocusCallback     = unsafePerformIO $ newIORef nullFunPtr
windowIconifyCallback   = unsafePerformIO $ newIORef nullFunPtr
framebufferSizeCallback = unsafePerformIO $ newIORef nullFunPtr
mouseButtonCallback     = unsafePerformIO $ newIORef nullFunPtr
cursorPosCallback       = unsafePerformIO $ newIORef nullFunPtr
cursorEnterCallback     = unsafePerformIO $ newIORef nullFunPtr
scrollCallback          = unsafePerformIO $ newIORef nullFunPtr
keyCallback             = unsafePerformIO $ newIORef nullFunPtr
charCallback            = unsafePerformIO $ newIORef nullFunPtr
monitorCallback         = unsafePerformIO $ newIORef nullFunPtr

setCallback
  :: (c -> IO (FunPtr c))          -- wrapper function
  -> (h -> c)                      -- adapter function
  -> (FunPtr c -> IO (FunPtr c))   -- glfwSet*Callback function
  -> IORef (FunPtr c)              -- storage location
  -> Maybe h                       -- Haskell callback
  -> IO ()
setCallback wf af gf ior mcb = do
    ccb <- maybe (return nullFunPtr) (wf . af) mcb
    _   <- gf ccb
    storeCallback ior ccb

storeCallback :: IORef (FunPtr a) -> FunPtr a -> IO ()
storeCallback ior new = do
    prev <- atomicModifyIORef' ior (\cur -> (new, cur))
    when (prev /= nullFunPtr) $ freeHaskellFunPtr prev

--------------------------------------------------------------------------------

-- see http://www.glfw.org/docs/3.0/window.html#window_hints_values
defaultWindowHints :: WindowHints
defaultWindowHints = WindowHints
    { windowHintsResizable           = True
    , windowHintsVisible             = True
    , windowHintsDecorated           = True
    , windowHintsRedBits             = 8
    , windowHintsGreenBits           = 8
    , windowHintsBlueBits            = 8
    , windowHintsAlphaBits           = 8
    , windowHintsDepthBits           = 24
    , windowHintsStencilBits         = 8
    , windowHintsAccumRedBits        = 0
    , windowHintsAccumGreenBits      = 0
    , windowHintsAccumBlueBits       = 0
    , windowHintsAccumAlphaBits      = 0
    , windowHintsAuxBuffers          = 0
    , windowHintsSamples             = 0
    , windowHintsRefreshRate         = 0
    , windowHintsStereo              = False
    , windowHintsSrgbCapable         = False
    , windowHintsClientApi           = OpenglApi
    , windowHintsContextVersionMajor = 1
    , windowHintsContextVersionMinor = 0
    , windowHintsContextRobustness   = NoRobustness
    , windowHintsOpenglForwardCompat = False
    , windowHintsOpenglDebugContext  = False
    , windowHintsOpenglProfile       = OpenglAnyProfile
    }

instance Storable GlfwVideoMode where
  sizeOf _ = (#const sizeof(GLFWvidmode))
  alignment _ = alignment (undefined :: CInt)
  peek ptr = do
      w  <- (#peek GLFWvidmode, width)       ptr :: IO CInt
      h  <- (#peek GLFWvidmode, height)      ptr :: IO CInt
      rb <- (#peek GLFWvidmode, redBits)     ptr :: IO CInt
      gb <- (#peek GLFWvidmode, greenBits)   ptr :: IO CInt
      bb <- (#peek GLFWvidmode, blueBits)    ptr :: IO CInt
      rr <- (#peek GLFWvidmode, refreshRate) ptr :: IO CInt
      return GlfwVideoMode
        { glfwVideoModeWidth       = w
        , glfwVideoModeHeight      = h
        , glfwVideoModeRedBits     = rb
        , glfwVideoModeGreenBits   = gb
        , glfwVideoModeBlueBits    = bb
        , glfwVideoModeRefreshRate = rr
        }

--------------------------------------------------------------------------------

initialize :: IO Bool
initialize =
    fromC `fmap` glfwInit

terminate :: IO ()
terminate =
    glfwTerminate

getVersion :: IO Version
getVersion =
    allocaArray 3 $ \p -> do
        let p0 = p
            p1 = p `advancePtr` 1
            p2 = p `advancePtr` 2
        glfwGetVersion p0 p1 p2
        v0 <- fromC `fmap` peek p0
        v1 <- fromC `fmap` peek p1
        v2 <- fromC `fmap` peek p2
        return $ Version v0 v1 v2

getVersionString :: IO String
getVersionString =
    glfwGetVersionString >>= peekCString

setErrorCallback :: Maybe ErrorCallback -> IO ()
setErrorCallback =
    setCallback
        wrapGlfwErrorCallback
        (\cb a0 a1 -> do
            s <- peekCString a1
            cb (fromC a0) s)
        glfwSetErrorCallback
        errorCallback

getMonitors :: IO (Maybe [Monitor])
getMonitors =
    alloca $ \pn -> do
        p <- glfwGetMonitors pn
        n <- fromC `fmap` peek pn
        if p == nullPtr || n <= 0
          then return Nothing
          else (Just . map Monitor) `fmap` peekArray n p

getPrimaryMonitor :: IO (Maybe Monitor)
getPrimaryMonitor = do
    p <- glfwGetPrimaryMonitor
    return $
      if p == nullPtr
        then Nothing
        else Just $ Monitor p

getMonitorPosition :: Monitor -> IO (Int, Int)
getMonitorPosition mon =
    allocaArray 2 $ \p -> do
        let px = p
            py = p `advancePtr` 1
        glfwGetMonitorPos (unMonitor mon) px py
        x <- fromC `fmap` peek px
        y <- fromC `fmap` peek py
        return (x, y)

getMonitorPhysicalSize :: Monitor -> IO (Int, Int)
getMonitorPhysicalSize mon =
    allocaArray 2 $ \p -> do
        let pw = p
            ph = p `advancePtr` 1
        glfwGetMonitorPhysicalSize (unMonitor mon) pw ph
        w <- fromC `fmap` peek pw
        h <- fromC `fmap` peek ph
        return (w, h)

getMonitorName :: Monitor -> IO (Maybe String)
getMonitorName mon = do
    p <- glfwGetMonitorName (unMonitor mon)
    if p == nullPtr
      then return Nothing
      else Just `fmap` peekCString p

setMonitorCallback :: Maybe MonitorCallback -> IO ()
setMonitorCallback mcb = do
    ccb <- case mcb of
        (Just cb) ->
            wrapGlfwMonitorCallback $ \cp ci ->
                cb (Monitor cp) (fromC ci)
        Nothing -> return nullFunPtr
    _ <- glfwSetMonitorCallback ccb
    storeCallback monitorCallback ccb

getVideoModes :: Monitor -> IO (Maybe [VideoMode])
getVideoModes mon =
    alloca $ \pn -> do
        p <- glfwGetVideoModes (unMonitor mon) pn
        n <- fromC `fmap` peek pn
        if p == nullPtr || n == 0
          then return Nothing
          else (Just . map fromC) `fmap` peekArray n p

getVideoMode :: Monitor -> IO (Maybe VideoMode)
getVideoMode mon = do
    p <- glfwGetVideoMode (unMonitor mon)
    if p == nullPtr
      then return Nothing
      else (Just . fromC) `fmap` peek p

setGamma :: Monitor -> Double -> IO ()
setGamma mon e =
    glfwSetGamma (unMonitor mon) (toC e)

setGammaRamp :: Monitor -> GammaRamp -> IO ()
setGammaRamp m gr =
    let r = map toC $ gammaRampRed   gr :: [CUShort]
        g = map toC $ gammaRampGreen gr :: [CUShort]
        b = map toC $ gammaRampBlue  gr :: [CUShort]
        n =     toC $ gammaRampSize  gr :: CUInt
    in allocaBytes (#size GLFWgammaramp) $ \pgr ->
       withArray r $ \pr ->
       withArray g $ \pg ->
       withArray b $ \pb -> do
           (#poke GLFWgammaramp, red)   pgr pr
           (#poke GLFWgammaramp, green) pgr pg
           (#poke GLFWgammaramp, blue)  pgr pb
           (#poke GLFWgammaramp, size)  pgr n
           glfwSetGammaRamp (unMonitor m) pgr

getGammaRamp :: Monitor -> IO (Maybe GammaRamp)
getGammaRamp m = do
    p <- glfwGetGammaRamp (unMonitor m)
    if p == nullPtr
      then return Nothing
      else do
          pr <- (#peek GLFWgammaramp, red)   p :: IO (Ptr CUShort)
          pg <- (#peek GLFWgammaramp, green) p :: IO (Ptr CUShort)
          pb <- (#peek GLFWgammaramp, blue)  p :: IO (Ptr CUShort)
          cn <- (#peek GLFWgammaramp, size)  p :: IO CUInt
          let n = fromC cn
          r <- map fromC `fmap` peekArray n pr
          g <- map fromC `fmap` peekArray n pg
          b <- map fromC `fmap` peekArray n pb
          return $ Just GammaRamp
            { gammaRampRed   = r
            , gammaRampGreen = g
            , gammaRampBlue  = b
            , gammaRampSize  = n
            }

-- XXX name exception
setDefaultWindowHints :: IO ()
setDefaultWindowHints = glfwDefaultWindowHints

-- XXX name exception
setWindowHints :: WindowHints -> IO ()
setWindowHints wh = do
    glfwWindowHint (#const GLFW_RESIZABLE)             $ toC $ windowHintsResizable           wh
    glfwWindowHint (#const GLFW_VISIBLE)               $ toC $ windowHintsVisible             wh
    glfwWindowHint (#const GLFW_DECORATED)             $ toC $ windowHintsDecorated           wh
    glfwWindowHint (#const GLFW_RED_BITS)              $ toC $ windowHintsRedBits             wh
    glfwWindowHint (#const GLFW_GREEN_BITS)            $ toC $ windowHintsGreenBits           wh
    glfwWindowHint (#const GLFW_BLUE_BITS)             $ toC $ windowHintsBlueBits            wh
    glfwWindowHint (#const GLFW_ALPHA_BITS)            $ toC $ windowHintsAlphaBits           wh
    glfwWindowHint (#const GLFW_DEPTH_BITS)            $ toC $ windowHintsDepthBits           wh
    glfwWindowHint (#const GLFW_STENCIL_BITS)          $ toC $ windowHintsStencilBits         wh
    glfwWindowHint (#const GLFW_ACCUM_RED_BITS)        $ toC $ windowHintsAccumRedBits        wh
    glfwWindowHint (#const GLFW_ACCUM_GREEN_BITS)      $ toC $ windowHintsAccumGreenBits      wh
    glfwWindowHint (#const GLFW_ACCUM_BLUE_BITS)       $ toC $ windowHintsAccumBlueBits       wh
    glfwWindowHint (#const GLFW_ACCUM_ALPHA_BITS)      $ toC $ windowHintsAccumAlphaBits      wh
    glfwWindowHint (#const GLFW_AUX_BUFFERS)           $ toC $ windowHintsAuxBuffers          wh
    glfwWindowHint (#const GLFW_SAMPLES)               $ toC $ windowHintsSamples             wh
    glfwWindowHint (#const GLFW_REFRESH_RATE)          $ toC $ windowHintsRefreshRate         wh
    glfwWindowHint (#const GLFW_STEREO)                $ toC $ windowHintsStereo              wh
    glfwWindowHint (#const GLFW_SRGB_CAPABLE)          $ toC $ windowHintsSrgbCapable         wh
    glfwWindowHint (#const GLFW_CLIENT_API)            $ toC $ windowHintsClientApi           wh
    glfwWindowHint (#const GLFW_CONTEXT_VERSION_MAJOR) $ toC $ windowHintsContextVersionMajor wh
    glfwWindowHint (#const GLFW_CONTEXT_VERSION_MINOR) $ toC $ windowHintsContextVersionMinor wh
    glfwWindowHint (#const GLFW_CONTEXT_ROBUSTNESS)    $ toC $ windowHintsContextRobustness   wh
    glfwWindowHint (#const GLFW_OPENGL_FORWARD_COMPAT) $ toC $ windowHintsOpenglForwardCompat wh
    glfwWindowHint (#const GLFW_OPENGL_DEBUG_CONTEXT)  $ toC $ windowHintsOpenglDebugContext  wh
    glfwWindowHint (#const GLFW_OPENGL_PROFILE)        $ toC $ windowHintsOpenglProfile       wh

createWindow :: Int -> Int -> String -> Maybe Monitor -> Maybe Window -> IO (Maybe Window)
createWindow width height title mmonitor mwindow =
    withCString title $ \ptitle -> do
        p <- glfwCreateWindow
            (toC width)
            (toC height)
            ptitle
            (maybe nullPtr unMonitor mmonitor)
            (maybe nullPtr unWindow mwindow)
        return $ if p == nullPtr
          then Nothing
          else Just $ Window p

destroyWindow :: Window -> IO ()
destroyWindow =
    glfwDestroyWindow . unWindow

windowShouldClose :: Window -> IO Bool
windowShouldClose win =
    fromC `fmap` glfwWindowShouldClose (unWindow win)

setWindowShouldClose :: Window -> Bool -> IO ()
setWindowShouldClose win b =
    glfwSetWindowShouldClose (unWindow win) (toC b)

setWindowTitle :: Window -> String -> IO ()
setWindowTitle win title =
    withCString title $ \ptitle ->
        glfwSetWindowTitle (unWindow win) ptitle

getWindowPos :: Window -> IO (Int, Int)
getWindowPos win =
    allocaArray 2 $ \pa -> do
        let px = pa
            py = pa `advancePtr` 1
        glfwGetWindowPos (unWindow win) px py
        x <- fromC `fmap` peek px
        y <- fromC `fmap` peek py
        return (x, y)

setWindowPos :: Window -> Int -> Int -> IO ()
setWindowPos win x y =
    glfwSetWindowPos (unWindow win) (toC x) (toC y)

getWindowSize :: Window -> IO (Int, Int)
getWindowSize win =
    allocaArray 2 $ \pa -> do
        let pwidth  = pa
            pheight = pa `advancePtr` 1
        glfwGetWindowSize (unWindow win) pwidth pheight
        width  <- fromC `fmap` peek pwidth
        height <- fromC `fmap` peek pheight
        return (width, height)

setWindowSize :: Window -> Int -> Int -> IO ()
setWindowSize win width height =
    glfwSetWindowSize (unWindow win) (toC width) (toC height)

getFramebufferSize :: Window -> IO (Int, Int)
getFramebufferSize win =
    allocaArray 2 $ \pa -> do
        let pwidth  = pa
            pheight = pa `advancePtr` 1
        glfwGetFramebufferSize (unWindow win) pwidth pheight
        width  <- fromC `fmap` peek pwidth
        height <- fromC `fmap` peek pheight
        return (width, height)

iconifyWindow :: Window -> IO ()
iconifyWindow win =
    glfwIconifyWindow (unWindow win)

restoreWindow :: Window -> IO ()
restoreWindow win =
    glfwRestoreWindow (unWindow win)

showWindow :: Window -> IO ()
showWindow win =
    glfwShowWindow (unWindow win)

hideWindow :: Window -> IO ()
hideWindow win =
    glfwHideWindow (unWindow win)

getWindowMonitor :: Window -> IO (Maybe Monitor)
getWindowMonitor win = do
    p <- glfwGetWindowMonitor (unWindow win)
    return $ if p == nullPtr
      then Nothing
      else Just $ Monitor p

getWindowAttrib :: Window -> WindowAttribute -> IO Bool
getWindowAttrib win wa =
    fromC `fmap` glfwGetWindowAttrib (unWindow win) (toC wa)

setWindowPosCallback :: Window -> Maybe WindowPosCallback -> IO ()
setWindowPosCallback w =
    setCallback
        wrapGlfwWindowPosCallback
        (\cb a0 a1 a2 ->
            cb (Window a0) (fromC a1) (fromC a2))
        (glfwSetWindowPosCallback (unWindow w))
        windowPosCallback

setWindowSizeCallback :: Window -> Maybe WindowSizeCallback -> IO ()
setWindowSizeCallback w =
    setCallback
        wrapGlfwWindowSizeCallback
        (\cb a0 a1 a2 ->
            cb (Window a0) (fromC a1) (fromC a2))
        (glfwSetWindowSizeCallback (unWindow w))
        windowSizeCallback

setWindowCloseCallback :: Window -> Maybe WindowCloseCallback -> IO ()
setWindowCloseCallback w =
    setCallback
        wrapGlfwWindowCloseCallback
        (. Window)
        (glfwSetWindowCloseCallback (unWindow w))
        windowCloseCallback

setWindowRefreshCallback :: Window -> Maybe WindowRefreshCallback -> IO ()
setWindowRefreshCallback w =
    setCallback
        wrapGlfwWindowRefreshCallback
        (. Window)
        (glfwSetWindowRefreshCallback (unWindow w))
        windowRefreshCallback

setWindowFocusCallback :: Window -> Maybe WindowFocusCallback -> IO ()
setWindowFocusCallback w =
    setCallback
        wrapGlfwWindowFocusCallback
        (\cb a0 a1 -> cb (Window a0) (fromC a1))
        (glfwSetWindowFocusCallback (unWindow w))
        windowFocusCallback

setWindowIconifyCallback :: Window -> Maybe WindowIconifyCallback -> IO ()
setWindowIconifyCallback w =
    setCallback
        wrapGlfwWindowIconifyCallback
        (\cb a0 a1 -> cb (Window a0) (fromC a1))
        (glfwSetWindowIconifyCallback (unWindow w))
        windowIconifyCallback

setFramebufferSizeCallback :: Window -> Maybe FramebufferSizeCallback -> IO ()
setFramebufferSizeCallback w =
    setCallback
        wrapGlfwFramebufferSizeCallback
        (\cb a0 a1 a2 -> cb (Window a0) (fromC a1) (fromC a2))
        (glfwSetFramebufferSizeCallback (unWindow w))
        framebufferSizeCallback

pollEvents :: IO ()
pollEvents = glfwPollEvents

waitEvents :: IO ()
waitEvents = glfwWaitEvents

-- instead of {g,s}etInputMode, we have {g,s}et{Cursor,Keys,MouseButtons}InputMode

getCursorInputMode :: Window -> IO CursorInputMode
getCursorInputMode w =
    fromC `fmap` glfwGetInputMode (unWindow w) (#const GLFW_CURSOR)

setCursorInputMode :: Window -> CursorInputMode -> IO ()
setCursorInputMode w m =
    glfwSetInputMode (unWindow w) (#const GLFW_CURSOR) (toC m)

getKeysInputMode :: Window -> IO KeysInputMode
getKeysInputMode w =
    fromC `fmap` glfwGetInputMode (unWindow w) (#const GLFW_STICKY_KEYS)

setKeysInputMode :: Window -> KeysInputMode -> IO ()
setKeysInputMode w m =
    glfwSetInputMode (unWindow w) (#const GLFW_STICKY_KEYS) (toC m)

getMouseButtonsInputMode :: Window -> IO MouseButtonsInputMode
getMouseButtonsInputMode w =
    fromC `fmap` glfwGetInputMode (unWindow w) (#const GLFW_STICKY_MOUSE_BUTTONS)

setMouseButtonsInputMode :: Window -> MouseButtonsInputMode -> IO ()
setMouseButtonsInputMode w m =
    glfwSetInputMode (unWindow w) (#const GLFW_STICKY_MOUSE_BUTTONS) (toC m)

getKey :: Window -> Key -> IO KeyAction
getKey w k =
    fromC `fmap` glfwGetKey (unWindow w) (toC k)

getMouseButton :: Window -> MouseButton -> IO MouseButtonAction
getMouseButton w b =
    fromC `fmap` glfwGetMouseButton (unWindow w) (toC b)

getCursorPos :: Window -> IO (Double, Double)
getCursorPos win =
    allocaArray 2 $ \pa -> do
        let px = pa
            py = pa `advancePtr` 1
        glfwGetCursorPos (unWindow win) px py
        x <- fromC `fmap` peek px
        y <- fromC `fmap` peek py
        return (x, y)

setCursorPos :: Window -> Double -> Double -> IO ()
setCursorPos win x y =
    glfwSetCursorPos (unWindow win) (toC x) (toC y)

setKeyCallback :: Window -> Maybe KeyCallback -> IO ()
setKeyCallback w =
    setCallback
        wrapGlfwKeyCallback
        (\cb a0 a1 a2 a3 a4 ->
            cb (Window a0) (fromC a1) (fromC a2) (fromC a3) (fromC a4))
        (glfwSetKeyCallback (unWindow w))
        keyCallback

setCharCallback :: Window -> Maybe CharCallback -> IO ()
setCharCallback w =
    setCallback
        wrapGlfwCharCallback
        (\cb a0 a1 -> cb (Window a0) (fromC a1))
        (glfwSetCharCallback (unWindow w))
        charCallback

setMouseButtonCallback :: Window -> Maybe MouseButtonCallback -> IO ()
setMouseButtonCallback w =
    setCallback
        wrapGlfwMouseButtonCallback
        (\cb a0 a1 a2 a3 -> cb (Window a0) (fromC a1) (fromC a2) (fromC a3))
        (glfwSetMouseButtonCallback (unWindow w))
        mouseButtonCallback

setCursorPosCallback :: Window -> Maybe CursorPosCallback -> IO ()
setCursorPosCallback w =
    setCallback
        wrapGlfwCursorPosCallback
        (\cb a0 a1 a2 -> cb (Window a0) (fromC a1) (fromC a2))
        (glfwSetCursorPosCallback (unWindow w))
        cursorPosCallback

setCursorEnterCallback :: Window -> Maybe CursorEnterCallback -> IO ()
setCursorEnterCallback w =
    setCallback
        wrapGlfwCursorEnterCallback
        (\cb a0 a1 -> cb (Window a0) (fromC a1))
        (glfwSetCursorEnterCallback (unWindow w))
        cursorEnterCallback

setScrollCallback :: Window -> Maybe ScrollCallback -> IO ()
setScrollCallback w =
    setCallback
        wrapGlfwScrollCallback
        (\cb a0 a1 a2 -> cb (Window a0) (fromC a1) (fromC a2))
        (glfwSetScrollCallback (unWindow w))
        scrollCallback

joystickPresent :: Joystick -> IO Bool
joystickPresent js =
    fromC `fmap` glfwJoystickPresent (toC js)

getJoystickAxes :: Joystick -> IO (Maybe [Double])
getJoystickAxes js =
    alloca $ \pn -> do
        p <- glfwGetJoystickAxes (toC js) pn
        n <- fromC `fmap` peek pn
        if p == nullPtr || n == 0
          then return Nothing
          else (Just . map fromC) `fmap` peekArray n p

getJoystickButtons :: Joystick -> IO (Maybe [JoystickButtonAction])
getJoystickButtons js =
    alloca $ \pn -> do
        p <- glfwGetJoystickButtons (toC js) pn
        n <- fromC `fmap` peek pn
        if p == nullPtr || n == 0
          then return Nothing
          else (Just . map fromC) `fmap` peekArray n p

getJoystickName :: Joystick -> IO (Maybe String)
getJoystickName js = do
    p <- glfwGetJoystickName (toC js)
    if p == nullPtr
      then return Nothing
      else Just `fmap` peekCString p

setClipboardString :: Window -> String -> IO ()
setClipboardString win s =
    withCString s (glfwSetClipboardString (unWindow win))

getClipboardString :: Window -> IO (Maybe String)
getClipboardString win = do
    p <- glfwGetClipboardString (unWindow win)
    if p == nullPtr
      then return Nothing
      else Just `fmap` peekCString p

getTime :: IO (Maybe Double)
getTime = do
    t <- fromC `fmap` glfwGetTime
    return $ if t == 0
      then Nothing
      else Just t

setTime :: Double -> IO ()
setTime =
    glfwSetTime . toC

makeContextCurrent :: Window -> IO ()
makeContextCurrent =
    glfwMakeContextCurrent . unWindow

getCurrentContext :: IO Window
getCurrentContext =
    Window `fmap` glfwGetCurrentContext

swapBuffers :: Window -> IO ()
swapBuffers =
    glfwSwapBuffers . unWindow

swapInterval :: Int -> IO ()
swapInterval =
    glfwSwapInterval . toC

extensionSupported :: String -> IO Bool
extensionSupported ext =
    withCString ext $ \pext ->
        fromC `fmap` glfwExtensionSupported pext

-- getProcAddress

--------------------------------------------------------------------------------

type ErrorCallback           = Error -> String                                            -> IO ()
type WindowPosCallback       = Window -> Int -> Int                                       -> IO ()
type WindowSizeCallback      = Window -> Int -> Int                                       -> IO ()
type WindowCloseCallback     = Window                                                     -> IO ()
type WindowRefreshCallback   = Window                                                     -> IO ()
type WindowFocusCallback     = Window -> FocusAction                                      -> IO ()
type WindowIconifyCallback   = Window -> IconifyAction                                    -> IO ()
type FramebufferSizeCallback = Window -> Int -> Int                                       -> IO ()
type MouseButtonCallback     = Window -> MouseButton -> MouseButtonAction -> ModifierKeys -> IO ()
type CursorPosCallback       = Window -> Double -> Double                                 -> IO ()
type CursorEnterCallback     = Window -> CursorAction                                     -> IO ()
type ScrollCallback          = Window -> Double -> Double                                 -> IO ()
type KeyCallback             = Window -> Key -> Int -> KeyAction -> ModifierKeys          -> IO ()
type CharCallback            = Window -> Char                                             -> IO ()
type MonitorCallback         = Monitor -> MonitorAction                                   -> IO ()
