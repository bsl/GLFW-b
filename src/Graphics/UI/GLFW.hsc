{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses    #-}
{-# LANGUAGE EmptyDataDecls           #-}

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
  , GammaRamp, gammaRampRed, gammaRampGreen, gammaRampBlue, gammaRampSize
  ) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Control.Monad         (when)
import Data.Bits             ((.&.))
import Data.Char             (chr, ord)
import Data.IORef            (IORef, atomicModifyIORef', newIORef)
import Foreign.C.String      (peekCString, withCString)
import Foreign.C.Types       (CChar, CDouble(..), CFloat(..), CInt(..), CUShort, CUChar(..), CUInt(..))
import Foreign.Marshal.Alloc (alloca, allocaBytes)
import Foreign.Marshal.Array (advancePtr, allocaArray, withArray, peekArray)
import Foreign.Ptr           (FunPtr, Ptr, freeHaskellFunPtr, nullFunPtr, nullPtr)
import Foreign.Storable      (Storable(..))
import System.IO.Unsafe      (unsafePerformIO)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

#include <GLFW/glfw3.h>

foreign import ccall glfwInit                   :: IO CInt
foreign import ccall glfwTerminate              :: IO ()
foreign import ccall glfwGetVersion             :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall glfwGetVersionString       :: IO (Ptr CChar)
foreign import ccall glfwSetErrorCallback       :: FunPtr GlfwErrorCallback -> IO (FunPtr GlfwErrorCallback)
foreign import ccall glfwGetMonitors            :: Ptr CInt -> IO (Ptr (Ptr GlfwMonitor))
foreign import ccall glfwGetPrimaryMonitor      :: IO (Ptr GlfwMonitor)
foreign import ccall glfwGetMonitorPos          :: Ptr GlfwMonitor -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall glfwGetMonitorPhysicalSize :: Ptr GlfwMonitor -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall glfwGetMonitorName         :: Ptr GlfwMonitor -> IO (Ptr CChar)
foreign import ccall glfwSetMonitorCallback     :: FunPtr GlfwMonitorCallback -> IO (FunPtr GlfwMonitorCallback)
foreign import ccall glfwGetVideoModes          :: Ptr GlfwMonitor -> Ptr CInt -> IO (Ptr VideoMode)
foreign import ccall glfwGetVideoMode           :: Ptr GlfwMonitor -> IO (Ptr VideoMode)
foreign import ccall glfwSetGamma               :: Ptr GlfwMonitor -> CFloat -> IO ()
foreign import ccall glfwSetGammaRamp         :: Ptr GlfwMonitor -> Ptr GlfwGammaRamp -> IO ()
foreign import ccall glfwGetGammaRamp         :: Ptr GlfwMonitor -> IO (Ptr GlfwGammaRamp)
foreign import ccall glfwDefaultWindowHints   :: IO ()
foreign import ccall glfwWindowHint           :: CInt -> CInt -> IO ()
foreign import ccall glfwCreateWindow         :: CInt -> CInt -> Ptr CChar -> Ptr GlfwMonitor -> Ptr GlfwWindow -> IO (Ptr GlfwWindow)
foreign import ccall glfwDestroyWindow        :: Ptr GlfwWindow -> IO ()
foreign import ccall glfwWindowShouldClose    :: Ptr GlfwWindow -> IO CInt
foreign import ccall glfwSetWindowShouldClose :: Ptr GlfwWindow -> CInt -> IO ()
foreign import ccall glfwSetWindowTitle       :: Ptr GlfwWindow -> Ptr CChar -> IO ()
foreign import ccall glfwGetWindowPos         :: Ptr GlfwWindow -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall glfwSetWindowPos         :: Ptr GlfwWindow -> CInt -> CInt -> IO ()
foreign import ccall glfwGetWindowSize        :: Ptr GlfwWindow -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall glfwSetWindowSize        :: Ptr GlfwWindow -> CInt -> CInt -> IO ()
foreign import ccall glfwGetFramebufferSize   :: Ptr GlfwWindow -> Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall glfwIconifyWindow        :: Ptr GlfwWindow -> IO ()
foreign import ccall glfwRestoreWindow        :: Ptr GlfwWindow -> IO ()
foreign import ccall glfwShowWindow           :: Ptr GlfwWindow -> IO ()
foreign import ccall glfwHideWindow           :: Ptr GlfwWindow -> IO ()
foreign import ccall glfwGetWindowMonitor     :: Ptr GlfwWindow -> IO (Ptr GlfwMonitor)
foreign import ccall glfwGetWindowAttrib      :: Ptr GlfwWindow -> CInt -> IO CInt
--foreign import ccall glfwSetWindowUserPointer
--foreign import ccall glfwGetWindowUserPointer
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
-- foreign import ccall glfwGetProcAddress

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

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class C c h where
  fromC :: c -> h
  toC   :: h -> c

instance C CInt Int where
  fromC = fromIntegral
  toC   = fromIntegral

instance C CUInt Int where
  fromC = fromIntegral
  toC   = fromIntegral

instance C CUShort Int where
  fromC = fromIntegral
  toC   = fromIntegral

instance C CInt Char where
  fromC = chr . fromIntegral
  toC   = fromIntegral . ord

instance C CDouble Double where
  fromC = realToFrac
  toC   = realToFrac

instance C CInt Bool where
  fromC (#const GL_TRUE)  = True
  fromC (#const GL_FALSE) = False
  fromC v = error $ "C CInt Bool fromC: " ++ show v
  toC _ = undefined  -- not needed

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

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

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

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
type CursorEnterCallback     = Window -> Bool                                             -> IO ()
type ScrollCallback          = Window -> Double -> Double                                 -> IO ()
type KeyCallback             = Window -> Key -> Int -> KeyAction -> ModifierKeys          -> IO ()
type CharCallback            = Window -> Char                                             -> IO ()
type MonitorCallback         = Monitor -> MonitorAction                                   -> IO ()

data GlfwGammaRamp

data GammaRamp = GammaRamp
  { gammaRampRed   :: [Int]
  , gammaRampGreen :: [Int]
  , gammaRampBlue  :: [Int]
  , gammaRampSize  :: Int
  } deriving (Eq, Show)

data CursorInputMode =
    CursorNormal
  | CursorHidden
  | CursorDisabled
  deriving (Eq, Show)

instance C CInt CursorInputMode where
  fromC (#const GLFW_CURSOR_NORMAL)   = CursorNormal
  fromC (#const GLFW_CURSOR_HIDDEN)   = CursorHidden
  fromC (#const GLFW_CURSOR_DISABLED) = CursorDisabled
  fromC v = error $ "C CInt CursorInputMode fromC: " ++ show v
  toC CursorNormal   = (#const GLFW_CURSOR_NORMAL)
  toC CursorHidden   = (#const GLFW_CURSOR_HIDDEN)
  toC CursorDisabled = (#const GLFW_CURSOR_DISABLED)

data KeysInputMode =
    KeysNormal
  | KeysSticky
  deriving (Eq, Show)

instance C CInt KeysInputMode where
  fromC (#const GL_TRUE)  = KeysSticky
  fromC (#const GL_FALSE) = KeysNormal
  fromC v = error $ "C CInt KeysInputMode fromC: " ++ show v
  toC KeysSticky = (#const GL_TRUE)
  toC KeysNormal = (#const GL_FALSE)

data MouseButtonsInputMode =
    MouseButtonsNormal
  | MouseButtonsSticky
  deriving (Eq, Show)

instance C CInt MouseButtonsInputMode where
  fromC (#const GL_TRUE)  = MouseButtonsSticky
  fromC (#const GL_FALSE) = MouseButtonsNormal
  fromC v = error $ "C CInt MouseButtonsInputMode fromC: " ++ show v
  toC MouseButtonsSticky = (#const GL_TRUE)
  toC MouseButtonsNormal = (#const GL_FALSE)

data WindowHints = WindowHints
  { windowHintsResizable           :: Bool
  , windowHintsVisible             :: Bool
  , windowHintsDecorated           :: Bool
  , windowHintsRedBits             :: Int
  , windowHintsGreenBits           :: Int
  , windowHintsBlueBits            :: Int
  , windowHintsAlphaBits           :: Int
  , windowHintsDepthBits           :: Int
  , windowHintsStencilBits         :: Int
  , windowHintsAccumRedBits        :: Int
  , windowHintsAccumGreenBits      :: Int
  , windowHintsAccumBlueBits       :: Int
  , windowHintsAccumAlphaBits      :: Int
  , windowHintsAuxBuffers          :: Int
  , windowHintsSamples             :: Int
  , windowHintsRefreshRate         :: Int
  , windowHintsStereo              :: Bool
  , windowHintsSrgbCapable         :: Bool
  , windowHintsClientApi           :: ClientApi
  , windowHintsContextVersionMajor :: Int
  , windowHintsContextVersionMinor :: Int
  , windowHintsContextRobustness   :: ContextRobustness
  , windowHintsOpenglForwardCompat :: Bool
  , windowHintsOpenglDebugContext  :: Bool
  , windowHintsOpenglProfile       :: OpenglProfile
  } deriving (Eq, Show)

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

data ClientApi =
    OpenglApi
  | OpenglEsApi
  deriving (Eq, Show)

instance C CInt ClientApi where
  fromC (#const GLFW_OPENGL_API)    = OpenglApi
  fromC (#const GLFW_OPENGL_ES_API) = OpenglEsApi
  fromC v = error $ "C CInt ClientApi fromC: " ++ show v
  toC OpenglApi   = (#const GLFW_OPENGL_API)
  toC OpenglEsApi = (#const GLFW_OPENGL_ES_API)

data ContextRobustness =
    NoRobustness
  | NoResetNotification
  | LoseContextOnReset
  deriving (Eq, Show)

instance C CInt ContextRobustness where
  fromC (#const GLFW_NO_ROBUSTNESS)         = NoRobustness
  fromC (#const GLFW_NO_RESET_NOTIFICATION) = NoResetNotification
  fromC (#const GLFW_LOSE_CONTEXT_ON_RESET) = LoseContextOnReset
  fromC v = error $ "C CInt ContextRobustness fromC: " ++ show v
  toC NoRobustness        = (#const GLFW_NO_ROBUSTNESS)
  toC NoResetNotification = (#const GLFW_NO_RESET_NOTIFICATION)
  toC LoseContextOnReset  = (#const GLFW_LOSE_CONTEXT_ON_RESET)

data OpenglProfile =
    OpenglAnyProfile
  | OpenglCompatProfile
  | OpenglCoreProfile
  deriving (Eq, Show)

instance C CInt OpenglProfile where
  fromC (#const GLFW_OPENGL_ANY_PROFILE)    = OpenglAnyProfile
  fromC (#const GLFW_OPENGL_COMPAT_PROFILE) = OpenglCompatProfile
  fromC (#const GLFW_OPENGL_CORE_PROFILE)   = OpenglCoreProfile
  fromC v = error $ "C CInt OpenglProfile fromC: " ++ show v
  toC OpenglAnyProfile    = (#const GLFW_OPENGL_ANY_PROFILE)
  toC OpenglCompatProfile = (#const GLFW_OPENGL_COMPAT_PROFILE)
  toC OpenglCoreProfile   = (#const GLFW_OPENGL_CORE_PROFILE)

data Error =
    NotInitialized
  | NoCurrentContext
  | InvalidEnum
  | InvalidValue
  | OutOfMemory
  | ApiUnavailable
  | VersionUnavailable
  | PlatformError
  | FormatUnavailable
  deriving (Eq, Show)

instance C CInt Error where
  fromC v = case v of
      (#const GLFW_NOT_INITIALIZED)     -> NotInitialized
      (#const GLFW_NO_CURRENT_CONTEXT)  -> NoCurrentContext
      (#const GLFW_INVALID_ENUM)        -> InvalidEnum
      (#const GLFW_INVALID_VALUE)       -> InvalidValue
      (#const GLFW_OUT_OF_MEMORY)       -> OutOfMemory
      (#const GLFW_API_UNAVAILABLE)     -> ApiUnavailable
      (#const GLFW_VERSION_UNAVAILABLE) -> VersionUnavailable
      (#const GLFW_PLATFORM_ERROR)      -> PlatformError
      (#const GLFW_FORMAT_UNAVAILABLE)  -> FormatUnavailable
      _ -> error $ "C CInt Error fromC: " ++ show v
  toC _ = undefined  -- not needed

data FocusAction =
    Focus
  | Defocus
  deriving (Eq, Show)

instance C CInt FocusAction where
  fromC (#const GL_TRUE)  = Focus
  fromC (#const GL_FALSE) = Defocus
  fromC v = error $ "C CInt FocusAction fromC: " ++ show v
  toC _ = undefined  -- not needed

data IconifyAction =
    Iconify
  | Restore
  deriving (Eq, Show)

instance C CInt IconifyAction where
  fromC (#const GL_TRUE)  = Iconify
  fromC (#const GL_FALSE) = Restore
  fromC v = error $ "C CInt IconifyAction fromC: " ++ show v
  toC _ = undefined  -- not needed

data KeyAction =
    KeyPress
  | KeyRelease
  | KeyRepeat
  deriving (Eq, Show)

instance C CInt KeyAction where
  fromC (#const GLFW_PRESS)   = KeyPress
  fromC (#const GLFW_RELEASE) = KeyRelease
  fromC (#const GLFW_REPEAT)  = KeyRepeat
  fromC v = error $ "C CInt KeyAction fromC: " ++ show v
  toC _ = undefined  -- not needed

data JoystickButtonAction =
    JoystickButtonPress
  | JoystickButtonRelease
  deriving (Eq, Show)

instance C CUChar JoystickButtonAction where
  fromC (#const GLFW_PRESS)   = JoystickButtonPress
  fromC (#const GLFW_RELEASE) = JoystickButtonRelease
  fromC v = error $ "C CInt JoystickButtonAction fromC: " ++ show v
  toC _ = undefined  -- not needed

data MouseButtonAction =
    MouseButtonPress
  | MouseButtonRelease
  deriving (Eq, Show)

instance C CInt MouseButtonAction where
  fromC (#const GLFW_PRESS)   = MouseButtonPress
  fromC (#const GLFW_RELEASE) = MouseButtonRelease
  fromC v = error $ "C CInt MouseButtonAction fromC: " ++ show v
  toC _ = undefined  -- not needed

data MonitorAction =
    Connect
  | Disconnect
  deriving (Eq, Show)

instance C CInt MonitorAction where
  fromC (#const GL_TRUE)  = Connect
  fromC (#const GL_FALSE) = Disconnect
  fromC v = error $ "C CInt Key fromC: " ++ show v
  toC _ = undefined  -- not needed

data ModifierKeys = ModifierKeys
  { modifierKeysShift   :: Bool
  , modifierKeysControl :: Bool
  , modifierKeysAlt     :: Bool
  , modifierKeysSuper   :: Bool
  } deriving (Eq, Show)

instance C CInt ModifierKeys where
  fromC v = ModifierKeys
    { modifierKeysShift   = (v .&. (#const GLFW_MOD_SHIFT))   /= 0
    , modifierKeysControl = (v .&. (#const GLFW_MOD_CONTROL)) /= 0
    , modifierKeysAlt     = (v .&. (#const GLFW_MOD_ALT))     /= 0
    , modifierKeysSuper   = (v .&. (#const GLFW_MOD_SUPER))   /= 0
    }
  toC _ = undefined  -- not needed

data Version = Version
  { versionMajor    :: Int
  , versionMinor    :: Int
  , versionRevision :: Int
  } deriving (Eq, Ord, Show)

data Key =
    KeyUnknown
  | KeySpace
  | KeyApostrophe
  | KeyComma
  | KeyMinus
  | KeyPeriod
  | KeySlash
  | Key0
  | Key1
  | Key2
  | Key3
  | Key4
  | Key5
  | Key6
  | Key7
  | Key8
  | Key9
  | KeySemicolon
  | KeyEqual
  | KeyA
  | KeyB
  | KeyC
  | KeyD
  | KeyE
  | KeyF
  | KeyG
  | KeyH
  | KeyI
  | KeyJ
  | KeyK
  | KeyL
  | KeyM
  | KeyN
  | KeyO
  | KeyP
  | KeyQ
  | KeyR
  | KeyS
  | KeyT
  | KeyU
  | KeyV
  | KeyW
  | KeyX
  | KeyY
  | KeyZ
  | KeyLeftBracket
  | KeyBackslash
  | KeyRightBracket
  | KeyGraveAccent
  | KeyWorld1
  | KeyWorld2
  | KeyEscape
  | KeyEnter
  | KeyTab
  | KeyBackspace
  | KeyInsert
  | KeyDelete
  | KeyRight
  | KeyLeft
  | KeyDown
  | KeyUp
  | KeyPageUp
  | KeyPageDown
  | KeyHome
  | KeyEnd
  | KeyCapsLock
  | KeyScrollLock
  | KeyNumLock
  | KeyPrintScreen
  | KeyPause
  | KeyF1
  | KeyF2
  | KeyF3
  | KeyF4
  | KeyF5
  | KeyF6
  | KeyF7
  | KeyF8
  | KeyF9
  | KeyF10
  | KeyF11
  | KeyF12
  | KeyF13
  | KeyF14
  | KeyF15
  | KeyF16
  | KeyF17
  | KeyF18
  | KeyF19
  | KeyF20
  | KeyF21
  | KeyF22
  | KeyF23
  | KeyF24
  | KeyF25
  | KeyPad0
  | KeyPad1
  | KeyPad2
  | KeyPad3
  | KeyPad4
  | KeyPad5
  | KeyPad6
  | KeyPad7
  | KeyPad8
  | KeyPad9
  | KeyPadDecimal
  | KeyPadDivide
  | KeyPadMultiply
  | KeyPadSubtract
  | KeyPadAdd
  | KeyPadEnter
  | KeyPadEqual
  | KeyLeftShift
  | KeyLeftControl
  | KeyLeftAlt
  | KeyLeftSuper
  | KeyRightShift
  | KeyRightControl
  | KeyRightAlt
  | KeyRightSuper
  | KeyMenu
  deriving (Eq, Show)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

instance C CInt Key where
  fromC v = case v of
     (#const GLFW_KEY_UNKNOWN)       -> KeyUnknown
     (#const GLFW_KEY_SPACE)         -> KeySpace
     (#const GLFW_KEY_APOSTROPHE)    -> KeyApostrophe
     (#const GLFW_KEY_COMMA)         -> KeyComma
     (#const GLFW_KEY_MINUS)         -> KeyMinus
     (#const GLFW_KEY_PERIOD)        -> KeyPeriod
     (#const GLFW_KEY_SLASH)         -> KeySlash
     (#const GLFW_KEY_0)             -> Key0
     (#const GLFW_KEY_1)             -> Key1
     (#const GLFW_KEY_2)             -> Key2
     (#const GLFW_KEY_3)             -> Key3
     (#const GLFW_KEY_4)             -> Key4
     (#const GLFW_KEY_5)             -> Key5
     (#const GLFW_KEY_6)             -> Key6
     (#const GLFW_KEY_7)             -> Key7
     (#const GLFW_KEY_8)             -> Key8
     (#const GLFW_KEY_9)             -> Key9
     (#const GLFW_KEY_SEMICOLON)     -> KeySemicolon
     (#const GLFW_KEY_EQUAL)         -> KeyEqual
     (#const GLFW_KEY_A)             -> KeyA
     (#const GLFW_KEY_B)             -> KeyB
     (#const GLFW_KEY_C)             -> KeyC
     (#const GLFW_KEY_D)             -> KeyD
     (#const GLFW_KEY_E)             -> KeyE
     (#const GLFW_KEY_F)             -> KeyF
     (#const GLFW_KEY_G)             -> KeyG
     (#const GLFW_KEY_H)             -> KeyH
     (#const GLFW_KEY_I)             -> KeyI
     (#const GLFW_KEY_J)             -> KeyJ
     (#const GLFW_KEY_K)             -> KeyK
     (#const GLFW_KEY_L)             -> KeyL
     (#const GLFW_KEY_M)             -> KeyM
     (#const GLFW_KEY_N)             -> KeyN
     (#const GLFW_KEY_O)             -> KeyO
     (#const GLFW_KEY_P)             -> KeyP
     (#const GLFW_KEY_Q)             -> KeyQ
     (#const GLFW_KEY_R)             -> KeyR
     (#const GLFW_KEY_S)             -> KeyS
     (#const GLFW_KEY_T)             -> KeyT
     (#const GLFW_KEY_U)             -> KeyU
     (#const GLFW_KEY_V)             -> KeyV
     (#const GLFW_KEY_W)             -> KeyW
     (#const GLFW_KEY_X)             -> KeyX
     (#const GLFW_KEY_Y)             -> KeyY
     (#const GLFW_KEY_Z)             -> KeyZ
     (#const GLFW_KEY_LEFT_BRACKET)  -> KeyLeftBracket
     (#const GLFW_KEY_BACKSLASH)     -> KeyBackslash
     (#const GLFW_KEY_RIGHT_BRACKET) -> KeyRightBracket
     (#const GLFW_KEY_GRAVE_ACCENT)  -> KeyGraveAccent
     (#const GLFW_KEY_WORLD_1)       -> KeyWorld1
     (#const GLFW_KEY_WORLD_2)       -> KeyWorld2
     (#const GLFW_KEY_ESCAPE)        -> KeyEscape
     (#const GLFW_KEY_ENTER)         -> KeyEnter
     (#const GLFW_KEY_TAB)           -> KeyTab
     (#const GLFW_KEY_BACKSPACE)     -> KeyBackspace
     (#const GLFW_KEY_INSERT)        -> KeyInsert
     (#const GLFW_KEY_DELETE)        -> KeyDelete
     (#const GLFW_KEY_RIGHT)         -> KeyRight
     (#const GLFW_KEY_LEFT)          -> KeyLeft
     (#const GLFW_KEY_DOWN)          -> KeyDown
     (#const GLFW_KEY_UP)            -> KeyUp
     (#const GLFW_KEY_PAGE_UP)       -> KeyPageUp
     (#const GLFW_KEY_PAGE_DOWN)     -> KeyPageDown
     (#const GLFW_KEY_HOME)          -> KeyHome
     (#const GLFW_KEY_END)           -> KeyEnd
     (#const GLFW_KEY_CAPS_LOCK)     -> KeyCapsLock
     (#const GLFW_KEY_SCROLL_LOCK)   -> KeyScrollLock
     (#const GLFW_KEY_NUM_LOCK)      -> KeyNumLock
     (#const GLFW_KEY_PRINT_SCREEN)  -> KeyPrintScreen
     (#const GLFW_KEY_PAUSE)         -> KeyPause
     (#const GLFW_KEY_F1)            -> KeyF1
     (#const GLFW_KEY_F2)            -> KeyF2
     (#const GLFW_KEY_F3)            -> KeyF3
     (#const GLFW_KEY_F4)            -> KeyF4
     (#const GLFW_KEY_F5)            -> KeyF5
     (#const GLFW_KEY_F6)            -> KeyF6
     (#const GLFW_KEY_F7)            -> KeyF7
     (#const GLFW_KEY_F8)            -> KeyF8
     (#const GLFW_KEY_F9)            -> KeyF9
     (#const GLFW_KEY_F10)           -> KeyF10
     (#const GLFW_KEY_F11)           -> KeyF11
     (#const GLFW_KEY_F12)           -> KeyF12
     (#const GLFW_KEY_F13)           -> KeyF13
     (#const GLFW_KEY_F14)           -> KeyF14
     (#const GLFW_KEY_F15)           -> KeyF15
     (#const GLFW_KEY_F16)           -> KeyF16
     (#const GLFW_KEY_F17)           -> KeyF17
     (#const GLFW_KEY_F18)           -> KeyF18
     (#const GLFW_KEY_F19)           -> KeyF19
     (#const GLFW_KEY_F20)           -> KeyF20
     (#const GLFW_KEY_F21)           -> KeyF21
     (#const GLFW_KEY_F22)           -> KeyF22
     (#const GLFW_KEY_F23)           -> KeyF23
     (#const GLFW_KEY_F24)           -> KeyF24
     (#const GLFW_KEY_F25)           -> KeyF25
     (#const GLFW_KEY_KP_0)          -> KeyPad0
     (#const GLFW_KEY_KP_1)          -> KeyPad1
     (#const GLFW_KEY_KP_2)          -> KeyPad2
     (#const GLFW_KEY_KP_3)          -> KeyPad3
     (#const GLFW_KEY_KP_4)          -> KeyPad4
     (#const GLFW_KEY_KP_5)          -> KeyPad5
     (#const GLFW_KEY_KP_6)          -> KeyPad6
     (#const GLFW_KEY_KP_7)          -> KeyPad7
     (#const GLFW_KEY_KP_8)          -> KeyPad8
     (#const GLFW_KEY_KP_9)          -> KeyPad9
     (#const GLFW_KEY_KP_DECIMAL)    -> KeyPadDecimal
     (#const GLFW_KEY_KP_DIVIDE)     -> KeyPadDivide
     (#const GLFW_KEY_KP_MULTIPLY)   -> KeyPadMultiply
     (#const GLFW_KEY_KP_SUBTRACT)   -> KeyPadSubtract
     (#const GLFW_KEY_KP_ADD)        -> KeyPadAdd
     (#const GLFW_KEY_KP_ENTER)      -> KeyPadEnter
     (#const GLFW_KEY_KP_EQUAL)      -> KeyPadEqual
     (#const GLFW_KEY_LEFT_SHIFT)    -> KeyLeftShift
     (#const GLFW_KEY_LEFT_CONTROL)  -> KeyLeftControl
     (#const GLFW_KEY_LEFT_ALT)      -> KeyLeftAlt
     (#const GLFW_KEY_LEFT_SUPER)    -> KeyLeftSuper
     (#const GLFW_KEY_RIGHT_SHIFT)   -> KeyRightShift
     (#const GLFW_KEY_RIGHT_CONTROL) -> KeyRightControl
     (#const GLFW_KEY_RIGHT_ALT)     -> KeyRightAlt
     (#const GLFW_KEY_RIGHT_SUPER)   -> KeyRightSuper
     (#const GLFW_KEY_MENU)          -> KeyMenu
     _ -> error $ "C CInt Key fromC: " ++ show v
  toC v = case v of
     KeyUnknown      -> (#const GLFW_KEY_UNKNOWN)
     KeySpace        -> (#const GLFW_KEY_SPACE)
     KeyApostrophe   -> (#const GLFW_KEY_APOSTROPHE)
     KeyComma        -> (#const GLFW_KEY_COMMA)
     KeyMinus        -> (#const GLFW_KEY_MINUS)
     KeyPeriod       -> (#const GLFW_KEY_PERIOD)
     KeySlash        -> (#const GLFW_KEY_SLASH)
     Key0            -> (#const GLFW_KEY_0)
     Key1            -> (#const GLFW_KEY_1)
     Key2            -> (#const GLFW_KEY_2)
     Key3            -> (#const GLFW_KEY_3)
     Key4            -> (#const GLFW_KEY_4)
     Key5            -> (#const GLFW_KEY_5)
     Key6            -> (#const GLFW_KEY_6)
     Key7            -> (#const GLFW_KEY_7)
     Key8            -> (#const GLFW_KEY_8)
     Key9            -> (#const GLFW_KEY_9)
     KeySemicolon    -> (#const GLFW_KEY_SEMICOLON)
     KeyEqual        -> (#const GLFW_KEY_EQUAL)
     KeyA            -> (#const GLFW_KEY_A)
     KeyB            -> (#const GLFW_KEY_B)
     KeyC            -> (#const GLFW_KEY_C)
     KeyD            -> (#const GLFW_KEY_D)
     KeyE            -> (#const GLFW_KEY_E)
     KeyF            -> (#const GLFW_KEY_F)
     KeyG            -> (#const GLFW_KEY_G)
     KeyH            -> (#const GLFW_KEY_H)
     KeyI            -> (#const GLFW_KEY_I)
     KeyJ            -> (#const GLFW_KEY_J)
     KeyK            -> (#const GLFW_KEY_K)
     KeyL            -> (#const GLFW_KEY_L)
     KeyM            -> (#const GLFW_KEY_M)
     KeyN            -> (#const GLFW_KEY_N)
     KeyO            -> (#const GLFW_KEY_O)
     KeyP            -> (#const GLFW_KEY_P)
     KeyQ            -> (#const GLFW_KEY_Q)
     KeyR            -> (#const GLFW_KEY_R)
     KeyS            -> (#const GLFW_KEY_S)
     KeyT            -> (#const GLFW_KEY_T)
     KeyU            -> (#const GLFW_KEY_U)
     KeyV            -> (#const GLFW_KEY_V)
     KeyW            -> (#const GLFW_KEY_W)
     KeyX            -> (#const GLFW_KEY_X)
     KeyY            -> (#const GLFW_KEY_Y)
     KeyZ            -> (#const GLFW_KEY_Z)
     KeyLeftBracket  -> (#const GLFW_KEY_LEFT_BRACKET)
     KeyBackslash    -> (#const GLFW_KEY_BACKSLASH)
     KeyRightBracket -> (#const GLFW_KEY_RIGHT_BRACKET)
     KeyGraveAccent  -> (#const GLFW_KEY_GRAVE_ACCENT)
     KeyWorld1       -> (#const GLFW_KEY_WORLD_1)
     KeyWorld2       -> (#const GLFW_KEY_WORLD_2)
     KeyEscape       -> (#const GLFW_KEY_ESCAPE)
     KeyEnter        -> (#const GLFW_KEY_ENTER)
     KeyTab          -> (#const GLFW_KEY_TAB)
     KeyBackspace    -> (#const GLFW_KEY_BACKSPACE)
     KeyInsert       -> (#const GLFW_KEY_INSERT)
     KeyDelete       -> (#const GLFW_KEY_DELETE)
     KeyRight        -> (#const GLFW_KEY_RIGHT)
     KeyLeft         -> (#const GLFW_KEY_LEFT)
     KeyDown         -> (#const GLFW_KEY_DOWN)
     KeyUp           -> (#const GLFW_KEY_UP)
     KeyPageUp       -> (#const GLFW_KEY_PAGE_UP)
     KeyPageDown     -> (#const GLFW_KEY_PAGE_DOWN)
     KeyHome         -> (#const GLFW_KEY_HOME)
     KeyEnd          -> (#const GLFW_KEY_END)
     KeyCapsLock     -> (#const GLFW_KEY_CAPS_LOCK)
     KeyScrollLock   -> (#const GLFW_KEY_SCROLL_LOCK)
     KeyNumLock      -> (#const GLFW_KEY_NUM_LOCK)
     KeyPrintScreen  -> (#const GLFW_KEY_PRINT_SCREEN)
     KeyPause        -> (#const GLFW_KEY_PAUSE)
     KeyF1           -> (#const GLFW_KEY_F1)
     KeyF2           -> (#const GLFW_KEY_F2)
     KeyF3           -> (#const GLFW_KEY_F3)
     KeyF4           -> (#const GLFW_KEY_F4)
     KeyF5           -> (#const GLFW_KEY_F5)
     KeyF6           -> (#const GLFW_KEY_F6)
     KeyF7           -> (#const GLFW_KEY_F7)
     KeyF8           -> (#const GLFW_KEY_F8)
     KeyF9           -> (#const GLFW_KEY_F9)
     KeyF10          -> (#const GLFW_KEY_F10)
     KeyF11          -> (#const GLFW_KEY_F11)
     KeyF12          -> (#const GLFW_KEY_F12)
     KeyF13          -> (#const GLFW_KEY_F13)
     KeyF14          -> (#const GLFW_KEY_F14)
     KeyF15          -> (#const GLFW_KEY_F15)
     KeyF16          -> (#const GLFW_KEY_F16)
     KeyF17          -> (#const GLFW_KEY_F17)
     KeyF18          -> (#const GLFW_KEY_F18)
     KeyF19          -> (#const GLFW_KEY_F19)
     KeyF20          -> (#const GLFW_KEY_F20)
     KeyF21          -> (#const GLFW_KEY_F21)
     KeyF22          -> (#const GLFW_KEY_F22)
     KeyF23          -> (#const GLFW_KEY_F23)
     KeyF24          -> (#const GLFW_KEY_F24)
     KeyF25          -> (#const GLFW_KEY_F25)
     KeyPad0         -> (#const GLFW_KEY_KP_0)
     KeyPad1         -> (#const GLFW_KEY_KP_1)
     KeyPad2         -> (#const GLFW_KEY_KP_2)
     KeyPad3         -> (#const GLFW_KEY_KP_3)
     KeyPad4         -> (#const GLFW_KEY_KP_4)
     KeyPad5         -> (#const GLFW_KEY_KP_5)
     KeyPad6         -> (#const GLFW_KEY_KP_6)
     KeyPad7         -> (#const GLFW_KEY_KP_7)
     KeyPad8         -> (#const GLFW_KEY_KP_8)
     KeyPad9         -> (#const GLFW_KEY_KP_9)
     KeyPadDecimal   -> (#const GLFW_KEY_KP_DECIMAL)
     KeyPadDivide    -> (#const GLFW_KEY_KP_DIVIDE)
     KeyPadMultiply  -> (#const GLFW_KEY_KP_MULTIPLY)
     KeyPadSubtract  -> (#const GLFW_KEY_KP_SUBTRACT)
     KeyPadAdd       -> (#const GLFW_KEY_KP_ADD)
     KeyPadEnter     -> (#const GLFW_KEY_KP_ENTER)
     KeyPadEqual     -> (#const GLFW_KEY_KP_EQUAL)
     KeyLeftShift    -> (#const GLFW_KEY_LEFT_SHIFT)
     KeyLeftControl  -> (#const GLFW_KEY_LEFT_CONTROL)
     KeyLeftAlt      -> (#const GLFW_KEY_LEFT_ALT)
     KeyLeftSuper    -> (#const GLFW_KEY_LEFT_SUPER)
     KeyRightShift   -> (#const GLFW_KEY_RIGHT_SHIFT)
     KeyRightControl -> (#const GLFW_KEY_RIGHT_CONTROL)
     KeyRightAlt     -> (#const GLFW_KEY_RIGHT_ALT)
     KeyRightSuper   -> (#const GLFW_KEY_RIGHT_SUPER)
     KeyMenu         -> (#const GLFW_KEY_MENU)

 -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data MouseButton =
    MouseButton1
  | MouseButton2
  | MouseButton3
  | MouseButton4
  | MouseButton5
  | MouseButton6
  | MouseButton7
  | MouseButton8
  deriving (Eq, Show)

instance C CInt MouseButton where
  fromC v = case v of
    (#const GLFW_MOUSE_BUTTON_1) -> MouseButton1
    (#const GLFW_MOUSE_BUTTON_2) -> MouseButton2
    (#const GLFW_MOUSE_BUTTON_3) -> MouseButton3
    (#const GLFW_MOUSE_BUTTON_4) -> MouseButton4
    (#const GLFW_MOUSE_BUTTON_5) -> MouseButton5
    (#const GLFW_MOUSE_BUTTON_6) -> MouseButton6
    (#const GLFW_MOUSE_BUTTON_7) -> MouseButton7
    (#const GLFW_MOUSE_BUTTON_8) -> MouseButton8
    _ -> error $ "C CInt MouseButton fromC: " ++ show v
  toC v = case v of
    MouseButton1 -> (#const GLFW_MOUSE_BUTTON_1)
    MouseButton2 -> (#const GLFW_MOUSE_BUTTON_2)
    MouseButton3 -> (#const GLFW_MOUSE_BUTTON_3)
    MouseButton4 -> (#const GLFW_MOUSE_BUTTON_4)
    MouseButton5 -> (#const GLFW_MOUSE_BUTTON_5)
    MouseButton6 -> (#const GLFW_MOUSE_BUTTON_6)
    MouseButton7 -> (#const GLFW_MOUSE_BUTTON_7)
    MouseButton8 -> (#const GLFW_MOUSE_BUTTON_8)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data GlfwWindow
newtype Window = Window { unWindow :: Ptr GlfwWindow }
  deriving Show

data GlfwMonitor
newtype Monitor = Monitor { unMonitor :: Ptr GlfwMonitor }
  deriving Show

data VideoMode = VideoMode
  { videoModeWidth       :: Int
  , videoModeHeight      :: Int
  , videoModeRedBits     :: Int
  , videoModeGreenBits   :: Int
  , videoModeBlueBits    :: Int
  , videoModeRefreshRate :: Int
  } deriving (Eq, Ord, Read, Show)

instance Storable VideoMode where
  sizeOf _ = (#const sizeof(GLFWvidmode))
  alignment _ = alignment (undefined :: CInt)
  peek ptr = do
      w  <- (#peek GLFWvidmode, width)       ptr :: IO CInt
      h  <- (#peek GLFWvidmode, height)      ptr :: IO CInt
      rb <- (#peek GLFWvidmode, redBits)     ptr :: IO CInt
      gb <- (#peek GLFWvidmode, greenBits)   ptr :: IO CInt
      bb <- (#peek GLFWvidmode, blueBits)    ptr :: IO CInt
      rr <- (#peek GLFWvidmode, refreshRate) ptr :: IO CInt
      return VideoMode
        { videoModeWidth       = fromIntegral w
        , videoModeHeight      = fromIntegral h
        , videoModeRedBits     = fromIntegral rb
        , videoModeGreenBits   = fromIntegral gb
        , videoModeBlueBits    = fromIntegral bb
        , videoModeRefreshRate = fromIntegral rr
        }

data WindowAttribute =
    Focused
  | Iconified
  | Visible
  | Resizable
  deriving (Eq, Show)

windowAttributeToCInt :: WindowAttribute -> CInt
windowAttributeToCInt wa = case wa of
    Focused   -> (#const GLFW_FOCUSED)
    Iconified -> (#const GLFW_ICONIFIED)
    Visible   -> (#const GLFW_VISIBLE)
    Resizable -> (#const GLFW_RESIZABLE)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

data Joystick =
    Joystick1
  | Joystick2
  | Joystick3
  | Joystick4
  | Joystick5
  | Joystick6
  | Joystick7
  | Joystick8
  | Joystick9
  | Joystick10
  | Joystick11
  | Joystick12
  | Joystick13
  | Joystick14
  | Joystick15
  | Joystick16
  deriving (Eq, Ord, Show)

instance C CInt Joystick where
  fromC v = case v of
      (#const GLFW_JOYSTICK_1)  -> Joystick1
      (#const GLFW_JOYSTICK_2)  -> Joystick2
      (#const GLFW_JOYSTICK_3)  -> Joystick3
      (#const GLFW_JOYSTICK_4)  -> Joystick4
      (#const GLFW_JOYSTICK_5)  -> Joystick5
      (#const GLFW_JOYSTICK_6)  -> Joystick6
      (#const GLFW_JOYSTICK_7)  -> Joystick7
      (#const GLFW_JOYSTICK_8)  -> Joystick8
      (#const GLFW_JOYSTICK_9)  -> Joystick9
      (#const GLFW_JOYSTICK_10) -> Joystick10
      (#const GLFW_JOYSTICK_11) -> Joystick11
      (#const GLFW_JOYSTICK_12) -> Joystick12
      (#const GLFW_JOYSTICK_13) -> Joystick13
      (#const GLFW_JOYSTICK_14) -> Joystick14
      (#const GLFW_JOYSTICK_15) -> Joystick15
      (#const GLFW_JOYSTICK_16) -> Joystick16
      _ -> error $ "C CInt Joystick fromC: " ++ show v
  toC v = case v of
      Joystick1  -> (#const GLFW_JOYSTICK_1)
      Joystick2  -> (#const GLFW_JOYSTICK_2)
      Joystick3  -> (#const GLFW_JOYSTICK_3)
      Joystick4  -> (#const GLFW_JOYSTICK_4)
      Joystick5  -> (#const GLFW_JOYSTICK_5)
      Joystick6  -> (#const GLFW_JOYSTICK_6)
      Joystick7  -> (#const GLFW_JOYSTICK_7)
      Joystick8  -> (#const GLFW_JOYSTICK_8)
      Joystick9  -> (#const GLFW_JOYSTICK_9)
      Joystick10 -> (#const GLFW_JOYSTICK_10)
      Joystick11 -> (#const GLFW_JOYSTICK_11)
      Joystick12 -> (#const GLFW_JOYSTICK_12)
      Joystick13 -> (#const GLFW_JOYSTICK_13)
      Joystick14 -> (#const GLFW_JOYSTICK_14)
      Joystick15 -> (#const GLFW_JOYSTICK_15)
      Joystick16 -> (#const GLFW_JOYSTICK_16)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

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
        v0 <- fromIntegral `fmap` peek p0
        v1 <- fromIntegral `fmap` peek p1
        v2 <- fromIntegral `fmap` peek p2
        return $ Version v0 v1 v2

getVersionString :: IO String
getVersionString =
    glfwGetVersionString >>= peekCString

setErrorCallback :: Maybe ErrorCallback -> IO ()
setErrorCallback =
    setCallback
        wrapGlfwErrorCallback
        (\cb -> (\a0 a1 -> do
            s <- peekCString a1
            cb (fromC a0) s))
        glfwSetErrorCallback
        errorCallback

getMonitors :: IO (Maybe [Monitor])
getMonitors =
    alloca $ \pn -> do
        p <- glfwGetMonitors pn
        n <- fromIntegral `fmap` peek pn
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
getMonitorPosition (Monitor gm) =
    allocaArray 2 $ \p -> do
        let px = p
            py = p `advancePtr` 1
        glfwGetMonitorPos gm px py
        x <- fromIntegral `fmap` peek px
        y <- fromIntegral `fmap` peek py
        return (x, y)

getMonitorPhysicalSize :: Monitor -> IO (Int, Int)
getMonitorPhysicalSize (Monitor gm) =
    allocaArray 2 $ \p -> do
        let pw = p
            ph = p `advancePtr` 1
        glfwGetMonitorPhysicalSize gm pw ph
        w <- fromIntegral `fmap` peek pw
        h <- fromIntegral `fmap` peek ph
        return (w, h)

getMonitorName :: Monitor -> IO (Maybe String)
getMonitorName (Monitor gm) = do
    p <- glfwGetMonitorName gm
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
getVideoModes (Monitor gm) =
    alloca $ \pn -> do
        p <- glfwGetVideoModes gm pn
        n <- fromIntegral `fmap` peek pn
        if p == nullPtr || n == 0
          then return Nothing
          else Just `fmap` peekArray n p

getVideoMode :: Monitor -> IO (Maybe VideoMode)
getVideoMode (Monitor gm) = do
    p <- glfwGetVideoMode gm
    if p == nullPtr
      then return Nothing
      else Just `fmap` peek p

setGamma :: Monitor -> Float -> IO ()
setGamma (Monitor gm) e =
    glfwSetGamma gm (realToFrac e)

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
          return $ Just $ GammaRamp
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

createWindow :: Int -> Int -> String -> Monitor -> Maybe Window -> IO (Maybe Window)
createWindow width height title monitor mwindow =
    withCString title $ \ptitle -> do
        p <- glfwCreateWindow (fromIntegral width)
                              (fromIntegral height)
                              ptitle
                              (unMonitor monitor)
                              (maybe nullPtr unWindow mwindow)
        return $ if p == nullPtr
          then Nothing
          else Just $ Window p

destroyWindow :: Window -> IO ()
destroyWindow =
    glfwDestroyWindow . unWindow

windowShouldClose :: Window -> IO Bool
windowShouldClose (Window pw) =
    fromC `fmap` glfwWindowShouldClose pw

setWindowShouldClose :: Window -> Bool -> IO ()
setWindowShouldClose (Window pw) b =
    glfwSetWindowShouldClose pw (toC b)

setWindowTitle :: Window -> String -> IO ()
setWindowTitle (Window pw) title =
    withCString title $ \ptitle ->
        glfwSetWindowTitle pw ptitle

getWindowPos :: Window -> IO (Int, Int)
getWindowPos (Window pw) =
    allocaArray 2 $ \pa -> do
        let px = pa
            py = pa `advancePtr` 1
        glfwGetWindowPos pw px py
        x <- fromIntegral `fmap` peek px
        y <- fromIntegral `fmap` peek py
        return (x, y)

setWindowPos :: Window -> Int -> Int -> IO ()
setWindowPos (Window pw) x y =
    glfwSetWindowPos pw (fromIntegral x) (fromIntegral y)

getWindowSize :: Window -> IO (Int, Int)
getWindowSize (Window pw) =
    allocaArray 2 $ \pa -> do
        let pwidth  = pa
            pheight = pa `advancePtr` 1
        glfwGetWindowSize pw pwidth pheight
        width  <- fromIntegral `fmap` peek pwidth
        height <- fromIntegral `fmap` peek pheight
        return (width, height)

setWindowSize :: Window -> Int -> Int -> IO ()
setWindowSize (Window pw) width height =
    glfwSetWindowSize pw (fromIntegral width) (fromIntegral height)

getFramebufferSize :: Window -> IO (Int, Int)
getFramebufferSize (Window pw) =
    allocaArray 2 $ \pa -> do
        let pwidth  = pa
            pheight = pa `advancePtr` 1
        glfwGetFramebufferSize pw pwidth pheight
        width  <- fromIntegral `fmap` peek pwidth
        height <- fromIntegral `fmap` peek pheight
        return (width, height)

iconifyWindow :: Window -> IO ()
iconifyWindow (Window pw) =
    glfwIconifyWindow pw

restoreWindow :: Window -> IO ()
restoreWindow (Window pw) =
    glfwRestoreWindow pw

showWindow :: Window -> IO ()
showWindow (Window pw) =
    glfwShowWindow pw

hideWindow :: Window -> IO ()
hideWindow (Window pw) =
    glfwHideWindow pw

getWindowMonitor :: Window -> IO (Maybe Monitor)
getWindowMonitor (Window pw) = do
    p <- glfwGetWindowMonitor pw
    return $ if p == nullPtr
      then Nothing
      else Just $ Monitor p

getWindowAttrib :: Window -> WindowAttribute -> IO Bool
getWindowAttrib (Window pw) wa =
    fromC `fmap` glfwGetWindowAttrib pw (windowAttributeToCInt wa)

setWindowPosCallback :: Window -> Maybe WindowPosCallback -> IO ()
setWindowPosCallback w =
    setCallback
        wrapGlfwWindowPosCallback
        (\cb -> (\a0 a1 a2 ->
            cb (Window a0) (fromIntegral a1) (fromIntegral a2)))
        (glfwSetWindowPosCallback (unWindow w))
        windowPosCallback

setWindowSizeCallback :: Window -> Maybe WindowSizeCallback -> IO ()
setWindowSizeCallback w =
    setCallback
        wrapGlfwWindowSizeCallback
        (\cb -> (\a0 a1 a2 ->
            cb (Window a0) (fromIntegral a1) (fromIntegral a2)))
        (glfwSetWindowSizeCallback (unWindow w))
        windowSizeCallback

setWindowCloseCallback :: Window -> Maybe WindowCloseCallback -> IO ()
setWindowCloseCallback w =
    setCallback
        wrapGlfwWindowCloseCallback
        (\cb -> (\a0 -> cb (Window a0)))
        (glfwSetWindowCloseCallback (unWindow w))
        windowCloseCallback

setWindowRefreshCallback :: Window -> Maybe WindowRefreshCallback -> IO ()
setWindowRefreshCallback w =
    setCallback
        wrapGlfwWindowRefreshCallback
        (\cb -> (\a0 -> cb (Window a0)))
        (glfwSetWindowRefreshCallback (unWindow w))
        windowRefreshCallback

setWindowFocusCallback :: Window -> Maybe WindowFocusCallback -> IO ()
setWindowFocusCallback w =
    setCallback
        wrapGlfwWindowFocusCallback
        (\cb -> (\a0 a1 -> cb (Window a0) (fromC a1)))
        (glfwSetWindowFocusCallback (unWindow w))
        windowFocusCallback

setWindowIconifyCallback :: Window -> Maybe WindowIconifyCallback -> IO ()
setWindowIconifyCallback w =
    setCallback
        wrapGlfwWindowIconifyCallback
        (\cb -> (\a0 a1 -> cb (Window a0) (fromC a1)))
        (glfwSetWindowIconifyCallback (unWindow w))
        windowIconifyCallback

setFramebufferSizeCallback :: Window -> Maybe FramebufferSizeCallback -> IO ()
setFramebufferSizeCallback w =
    setCallback
        wrapGlfwFramebufferSizeCallback
        (\cb -> (\a0 a1 a2 -> cb (Window a0) (fromIntegral a1) (fromIntegral a2)))
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
getCursorPos (Window pw) =
    allocaArray 2 $ \pa -> do
        let px = pa
            py = pa `advancePtr` 1
        glfwGetCursorPos pw px py
        x <- realToFrac `fmap` peek px
        y <- realToFrac `fmap` peek py
        return (x, y)

setCursorPos :: Window -> Double -> Double -> IO ()
setCursorPos (Window pw) x y =
    glfwSetCursorPos pw (realToFrac x) (realToFrac y)

setKeyCallback :: Window -> Maybe KeyCallback -> IO ()
setKeyCallback w =
    setCallback
        wrapGlfwKeyCallback
        (\cb -> (\a0 a1 a2 a3 a4 ->
            cb (Window a0) (fromC a1) (fromC a2) (fromC a3) (fromC a4)))
        (glfwSetKeyCallback (unWindow w))
        keyCallback

setCharCallback :: Window -> Maybe CharCallback -> IO ()
setCharCallback w =
    setCallback
        wrapGlfwCharCallback
        (\cb -> (\a0 a1 -> cb (Window a0) (fromC a1)))
        (glfwSetCharCallback (unWindow w))
        charCallback

setMouseButtonCallback :: Window -> Maybe MouseButtonCallback -> IO ()
setMouseButtonCallback w =
    setCallback
        wrapGlfwMouseButtonCallback
        (\cb -> (\a0 a1 a2 a3 -> cb (Window a0) (fromC a1) (fromC a2) (fromC a3)))
        (glfwSetMouseButtonCallback (unWindow w))
        mouseButtonCallback

setCursorPosCallback :: Window -> Maybe CursorPosCallback -> IO ()
setCursorPosCallback w =
    setCallback
        wrapGlfwCursorPosCallback
        (\cb -> (\a0 a1 a2 -> cb (Window a0) (fromC a1) (fromC a2)))
        (glfwSetCursorPosCallback (unWindow w))
        cursorPosCallback

setCursorEnterCallback :: Window -> Maybe CursorEnterCallback -> IO ()
setCursorEnterCallback w =
    setCallback
        wrapGlfwCursorEnterCallback
        (\cb -> (\a0 a1 -> cb (Window a0) (fromC a1)))
        (glfwSetCursorEnterCallback (unWindow w))
        cursorEnterCallback

setScrollCallback :: Window -> Maybe ScrollCallback -> IO ()
setScrollCallback w =
    setCallback
        wrapGlfwScrollCallback
        (\cb -> (\a0 a1 a2 -> cb (Window a0) (fromC a1) (fromC a2)))
        (glfwSetScrollCallback (unWindow w))
        scrollCallback

joystickPresent :: Joystick -> IO Bool
joystickPresent js =
    fromC `fmap` glfwJoystickPresent (toC js)

getJoystickAxes :: Joystick -> IO (Maybe [Float])
getJoystickAxes js =
    alloca $ \pn -> do
        p <- glfwGetJoystickAxes (toC js) pn
        n <- fromC `fmap` peek pn
        if p == nullPtr || n == 0
          then return Nothing
          else (Just . map realToFrac) `fmap` peekArray n p

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
setClipboardString (Window pw) s =
    withCString s (glfwSetClipboardString pw)

getClipboardString :: Window -> IO (Maybe String)
getClipboardString (Window pw) = do
    p <- glfwGetClipboardString pw
    if p == nullPtr
      then return Nothing
      else Just `fmap` peekCString p

getTime :: IO (Maybe Double)
getTime = do
    t <- realToFrac `fmap` glfwGetTime
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
