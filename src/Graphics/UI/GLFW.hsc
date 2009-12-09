{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses    #-}

module Graphics.UI.GLFW
  ( -- *   Initialization and termination
    initialize
  , terminate

    -- *   Video mode information
  , getVideoMode
  , getVideoModes
    --
  , VideoMode(..)

    -- *   Windows
    -- **  Management
  , openWindow
  , closeWindow
  , setWindowTitle
  , setWindowDimensions
  , setWindowPosition
  , iconifyWindow
  , restoreWindow
  , swapWindowBuffers
  , setWindowBufferSwapInterval
    --
  , DisplayMode(..)
  , DisplayOptions(..)
  , defaultDisplayOptions
    -- **  Information
  , windowIsOpen
  , windowIsActive
  , windowIsIconified
  , windowIsResizable
  , windowIsHardwareAccelerated
  , windowSupportsStereoRendering
  , getWindowRefreshRate
  , getWindowDimensions
  , getWindowValue
  , setWindowCloseCallback
  , setWindowResizeCallback
  , setWindowRefreshCallback
    --
  , WindowValue(..)
  , WindowCloseCallback
  , WindowResizeCallback
  , WindowRefreshCallback

    -- *   Input
  , pollEvents
  , waitEvents
    -- **  Keyboard
  , keyboardKeyIsPressed
  , setKeyboardKeyCallback
  , setKeyboardCharCallback
    --
  , KeyboardKey(..)
  , KeyboardKeyCallback
  , KeyboardCharCallback
    -- **  Mouse
  , mouseButtonIsPressed
  , getMousePosition
  , getMouseWheel
  , setMousePosition
  , setMouseWheel
  , setMouseButtonCallback
  , setMousePositionCallback
  , setMouseWheelCallback
    --
  , MouseButton(..)
  , MouseButtonCallback
  , MousePositionCallback
  , MouseWheelCallback
    -- **  Joystick
  , joystickIsPresent
  , getJoystickPosition
  , getNumJoystickAxes
  , getNumJoystickButtons
  , joystickButtonsArePressed
    --
  , Joystick(..)

    -- *   Time
  , getTime
  , setTime
  , resetTime
  , sleep

    -- *   Version information
  , getGlfwVersion
  , getGlVersion
  ) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Control.Monad         (when)
import Data.Char             (chr, ord)
import Data.IORef            (IORef, atomicModifyIORef, newIORef)
import Data.Maybe            (fromJust, isJust)
import Data.Version          (Version(..))
import Foreign.C.String      (CString, withCString)
import Foreign.C.Types       (CDouble, CFloat, CInt, CUChar)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray)
import Foreign.Ptr           (FunPtr, Ptr, freeHaskellFunPtr)
import Foreign.Storable      (Storable(..))
import System.IO.Unsafe      (unsafePerformIO)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

#include <GL/glfw.h>

foreign import ccall unsafe glfwInit                     :: IO CInt
foreign import ccall unsafe glfwTerminate                :: IO ()
foreign import ccall unsafe glfwGetVersion               :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

foreign import ccall unsafe glfwOpenWindow               :: CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe glfwOpenWindowHint           :: CInt -> CInt -> IO ()
foreign import ccall unsafe glfwCloseWindow              :: IO ()
foreign import ccall unsafe glfwSetWindowCloseCallback   :: FunPtr GlfwWindowCloseCallback -> IO ()
foreign import ccall unsafe glfwSetWindowTitle           :: CString -> IO ()
foreign import ccall unsafe glfwSetWindowSize            :: CInt -> CInt -> IO ()
foreign import ccall unsafe glfwSetWindowPos             :: CInt -> CInt -> IO ()
foreign import ccall unsafe glfwGetWindowSize            :: Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall        glfwSetWindowSizeCallback    :: FunPtr GlfwWindowResizeCallback -> IO ()
foreign import ccall unsafe glfwIconifyWindow            :: IO ()
foreign import ccall unsafe glfwRestoreWindow            :: IO ()
foreign import ccall unsafe glfwGetWindowParam           :: CInt -> IO CInt
foreign import ccall        glfwSwapBuffers              :: IO ()
foreign import ccall unsafe glfwSwapInterval             :: CInt -> IO ()
foreign import ccall unsafe glfwSetWindowRefreshCallback :: FunPtr GlfwWindowRefreshCallback -> IO ()

foreign import ccall unsafe glfwGetVideoModes            :: Ptr VideoMode -> CInt -> IO CInt
foreign import ccall unsafe glfwGetDesktopMode           :: Ptr VideoMode -> IO ()

foreign import ccall unsafe glfwPollEvents               :: IO ()
foreign import ccall unsafe glfwWaitEvents               :: IO ()
foreign import ccall unsafe glfwGetKey                   :: CInt -> IO CInt
foreign import ccall unsafe glfwGetMouseButton           :: CInt -> IO CInt
foreign import ccall unsafe glfwGetMousePos              :: Ptr CInt -> Ptr CInt -> IO ()
foreign import ccall unsafe glfwSetMousePos              :: CInt -> CInt -> IO ()
foreign import ccall unsafe glfwGetMouseWheel            :: IO CInt
foreign import ccall unsafe glfwSetMouseWheel            :: CInt -> IO ()
foreign import ccall        glfwSetKeyCallback           :: FunPtr GlfwKeyboardKeyCallback -> IO ()
foreign import ccall        glfwSetCharCallback          :: FunPtr GlfwKeyboardCharCallback -> IO ()
foreign import ccall        glfwSetMouseButtonCallback   :: FunPtr GlfwMouseButtonCallback -> IO ()
foreign import ccall        glfwSetMousePosCallback      :: FunPtr GlfwMousePositionCallback -> IO ()
foreign import ccall        glfwSetMouseWheelCallback    :: FunPtr GlfwMouseWheelCallback -> IO ()
foreign import ccall unsafe glfwGetJoystickParam         :: CInt -> CInt -> IO CInt
foreign import ccall unsafe glfwGetJoystickPos           :: CInt -> Ptr CFloat -> CInt -> IO CInt
foreign import ccall unsafe glfwGetJoystickButtons       :: CInt -> Ptr CUChar -> CInt -> IO CInt

foreign import ccall unsafe glfwGetTime                  :: IO CDouble
foreign import ccall unsafe glfwSetTime                  :: CDouble -> IO ()
foreign import ccall unsafe glfwSleep                    :: CDouble -> IO ()

foreign import ccall unsafe glfwGetGLVersion             :: Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

type GlfwKeyboardCharCallback  = CInt -> CInt -> IO ()
type GlfwKeyboardKeyCallback   = CInt -> CInt -> IO ()
type GlfwMouseButtonCallback   = CInt -> CInt -> IO ()
type GlfwMousePositionCallback = CInt -> CInt -> IO ()
type GlfwMouseWheelCallback    = CInt         -> IO ()
type GlfwWindowCloseCallback   =                 IO CInt
type GlfwWindowRefreshCallback =                 IO ()
type GlfwWindowResizeCallback  = CInt -> CInt -> IO ()

type KeyboardCharCallback  = Char -> Bool        -> IO ()
type KeyboardKeyCallback   = KeyboardKey -> Bool -> IO ()
type MouseButtonCallback   = MouseButton -> Bool -> IO ()
type MousePositionCallback = Int -> Int          -> IO ()
type MouseWheelCallback    = Int                 -> IO ()
type WindowCloseCallback   =                        IO Bool
type WindowRefreshCallback =                        IO ()
type WindowResizeCallback  = Int -> Int          -> IO ()

foreign import ccall unsafe "wrapper" wrapKeyboardCharCallback  :: GlfwKeyboardCharCallback  -> IO (FunPtr GlfwKeyboardCharCallback)
foreign import ccall unsafe "wrapper" wrapKeyboardKeyCallback   :: GlfwKeyboardKeyCallback   -> IO (FunPtr GlfwKeyboardKeyCallback)
foreign import ccall unsafe "wrapper" wrapMouseButtonCallback   :: GlfwMouseButtonCallback   -> IO (FunPtr GlfwMouseButtonCallback)
foreign import ccall unsafe "wrapper" wrapMousePositionCallback :: GlfwMousePositionCallback -> IO (FunPtr GlfwMousePositionCallback)
foreign import ccall unsafe "wrapper" wrapMouseWheelCallback    :: GlfwMouseWheelCallback    -> IO (FunPtr GlfwMouseWheelCallback)
foreign import ccall unsafe "wrapper" wrapWindowCloseCallback   :: GlfwWindowCloseCallback   -> IO (FunPtr GlfwWindowCloseCallback)
foreign import ccall unsafe "wrapper" wrapWindowRefreshCallback :: GlfwWindowRefreshCallback -> IO (FunPtr GlfwWindowRefreshCallback)
foreign import ccall unsafe "wrapper" wrapWindowResizeCallback  :: GlfwWindowResizeCallback  -> IO (FunPtr GlfwWindowResizeCallback)

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Initialization and termination

initialize :: IO Bool
initialize =
    fromC `fmap` glfwInit

terminate :: IO ()
terminate =
    glfwTerminate

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Video mode information

getVideoMode :: IO VideoMode
getVideoMode =
    alloca $ \ptr -> do
        glfwGetDesktopMode ptr
        peek ptr

getVideoModes :: IO [VideoMode]
getVideoModes =
    allocaArray m $ \ptr -> do
        n <- glfwGetVideoModes ptr (toC m)
        peekArray (fromC n) ptr
  where
    m = 256

-- -- -- -- -- -- -- -- -- --

data VideoMode = VideoMode
  { videoMode_width        :: Int
  , videoMode_height       :: Int
  , videoMode_numRedBits   :: Int
  , videoMode_numGreenBits :: Int
  , videoMode_numBlueBits  :: Int
  } deriving Show

instance Storable VideoMode where
  sizeOf    _ = (#const sizeof(GLFWvidmode))
  alignment _ = alignment (undefined :: CInt)

  peek ptr = do
      w <- (#peek GLFWvidmode, Width)     ptr :: IO CInt
      h <- (#peek GLFWvidmode, Height)    ptr :: IO CInt
      r <- (#peek GLFWvidmode, RedBits)   ptr :: IO CInt
      g <- (#peek GLFWvidmode, GreenBits) ptr :: IO CInt
      b <- (#peek GLFWvidmode, BlueBits)  ptr :: IO CInt
      return VideoMode
        { videoMode_width        = fromC w
        , videoMode_height       = fromC h
        , videoMode_numRedBits   = fromC r
        , videoMode_numGreenBits = fromC g
        , videoMode_numBlueBits  = fromC b
        }

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Window management

openWindow :: DisplayOptions -> IO Bool
openWindow displayOptions = do
    let DisplayOptions
          { displayOptions_width               = _displayOptions_width
          , displayOptions_height              = _displayOptions_height
          , displayOptions_numRedBits          = _displayOptions_numRedBits
          , displayOptions_numGreenBits        = _displayOptions_numGreenBits
          , displayOptions_numBlueBits         = _displayOptions_numBlueBits
          , displayOptions_numAlphaBits        = _displayOptions_numAlphaBits
          , displayOptions_numDepthBits        = _displayOptions_numDepthBits
          , displayOptions_numStencilBits      = _displayOptions_numStencilBits
          , displayOptions_displayMode         = _displayOptions_displayMode
          , displayOptions_refreshRate         = _displayOptions_refreshRate
          , displayOptions_accumNumRedBits     = _displayOptions_accumNumRedBits
          , displayOptions_accumNumGreenBits   = _displayOptions_accumNumGreenBits
          , displayOptions_accumNumBlueBits    = _displayOptions_accumNumBlueBits
          , displayOptions_accumNumAlphaBits   = _displayOptions_accumNumAlphaBits
          , displayOptions_numAuxiliaryBuffers = _displayOptions_numAuxiliaryBuffers
          , displayOptions_numFsaaSamples      = _displayOptions_numFsaaSamples
          , displayOptions_windowIsResizable   = _displayOptions_windowIsResizable
          , displayOptions_stereoRendering     = _displayOptions_stereoRendering
          } = displayOptions

    -- Add hints.
    when (isJust _displayOptions_refreshRate)         $ glfwOpenWindowHint (#const GLFW_REFRESH_RATE)     (toC (fromJust _displayOptions_refreshRate))
    when (isJust _displayOptions_accumNumRedBits)     $ glfwOpenWindowHint (#const GLFW_ACCUM_RED_BITS)   (toC (fromJust _displayOptions_accumNumRedBits))
    when (isJust _displayOptions_accumNumGreenBits)   $ glfwOpenWindowHint (#const GLFW_ACCUM_GREEN_BITS) (toC (fromJust _displayOptions_accumNumGreenBits))
    when (isJust _displayOptions_accumNumBlueBits)    $ glfwOpenWindowHint (#const GLFW_ACCUM_BLUE_BITS)  (toC (fromJust _displayOptions_accumNumBlueBits))
    when (isJust _displayOptions_accumNumAlphaBits)   $ glfwOpenWindowHint (#const GLFW_ACCUM_ALPHA_BITS) (toC (fromJust _displayOptions_accumNumAlphaBits))
    when (isJust _displayOptions_numAuxiliaryBuffers) $ glfwOpenWindowHint (#const GLFW_AUX_BUFFERS)      (toC (fromJust _displayOptions_numAuxiliaryBuffers))
    when (isJust _displayOptions_numFsaaSamples)      $ glfwOpenWindowHint (#const GLFW_FSAA_SAMPLES)     (toC (fromJust _displayOptions_numFsaaSamples))

    glfwOpenWindowHint (#const GLFW_WINDOW_NO_RESIZE) (toC (not _displayOptions_windowIsResizable))
    glfwOpenWindowHint (#const GLFW_STEREO)           (toC      _displayOptions_stereoRendering)

    -- Open the window.
    fromC `fmap` glfwOpenWindow
      (toC _displayOptions_width)
      (toC _displayOptions_height)
      (toC _displayOptions_numRedBits)
      (toC _displayOptions_numGreenBits)
      (toC _displayOptions_numBlueBits)
      (toC _displayOptions_numAlphaBits)
      (toC _displayOptions_numDepthBits)
      (toC _displayOptions_numStencilBits)
      (toC _displayOptions_displayMode)

closeWindow :: IO ()
closeWindow =
    glfwCloseWindow

setWindowTitle :: String -> IO ()
setWindowTitle t =
    withCString t glfwSetWindowTitle

setWindowDimensions :: Int -> Int -> IO ()
setWindowDimensions w h =
    glfwSetWindowSize (toC w) (toC h)

setWindowPosition :: Int -> Int -> IO ()
setWindowPosition w h =
    glfwSetWindowPos (toC w) (toC h)

iconifyWindow :: IO ()
iconifyWindow =
    glfwIconifyWindow

restoreWindow :: IO ()
restoreWindow =
    glfwRestoreWindow

swapWindowBuffers :: IO ()
swapWindowBuffers =
    glfwSwapBuffers

setWindowBufferSwapInterval :: Int -> IO ()
setWindowBufferSwapInterval =
    glfwSwapInterval . toC

-- -- -- -- -- -- -- -- -- --

data DisplayMode
  = Window
  | Fullscreen
  deriving (Show)

instance C DisplayMode CInt where
  toC dm = case dm of
      Window     -> #const GLFW_WINDOW
      Fullscreen -> #const GLFW_FULLSCREEN

  fromC i = case i of
      (#const GLFW_WINDOW    ) -> Window
      (#const GLFW_FULLSCREEN) -> Fullscreen
      _                        -> makeFromCError "DisplayMode" i

-- -- -- -- -- -- -- -- -- --

data DisplayOptions = DisplayOptions
  { displayOptions_width               :: Int
  , displayOptions_height              :: Int
  , displayOptions_numRedBits          :: Int
  , displayOptions_numGreenBits        :: Int
  , displayOptions_numBlueBits         :: Int
  , displayOptions_numAlphaBits        :: Int
  , displayOptions_numDepthBits        :: Int
  , displayOptions_numStencilBits      :: Int
  , displayOptions_displayMode         :: DisplayMode
  , displayOptions_refreshRate         :: Maybe Int
  , displayOptions_accumNumRedBits     :: Maybe Int
  , displayOptions_accumNumGreenBits   :: Maybe Int
  , displayOptions_accumNumBlueBits    :: Maybe Int
  , displayOptions_accumNumAlphaBits   :: Maybe Int
  , displayOptions_numAuxiliaryBuffers :: Maybe Int
  , displayOptions_numFsaaSamples      :: Maybe Int
  , displayOptions_windowIsResizable   :: Bool
  , displayOptions_stereoRendering     :: Bool
  } deriving (Show)

defaultDisplayOptions :: DisplayOptions
defaultDisplayOptions =
    DisplayOptions
      { displayOptions_width               = 0
      , displayOptions_height              = 0
      , displayOptions_numRedBits          = 0
      , displayOptions_numGreenBits        = 0
      , displayOptions_numBlueBits         = 0
      , displayOptions_numAlphaBits        = 0
      , displayOptions_numDepthBits        = 0
      , displayOptions_numStencilBits      = 0
      , displayOptions_displayMode         = Window
      , displayOptions_refreshRate         = Nothing
      , displayOptions_accumNumRedBits     = Nothing
      , displayOptions_accumNumGreenBits   = Nothing
      , displayOptions_accumNumBlueBits    = Nothing
      , displayOptions_accumNumAlphaBits   = Nothing
      , displayOptions_numAuxiliaryBuffers = Nothing
      , displayOptions_numFsaaSamples      = Nothing
      , displayOptions_windowIsResizable   = True
      , displayOptions_stereoRendering     = False
      }

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Window information

windowIsOpen :: IO Bool
windowIsOpen =
    fromC `fmap` glfwGetWindowParam (#const GLFW_OPENED)

windowIsActive :: IO Bool
windowIsActive =
    fromC `fmap` glfwGetWindowParam (#const GLFW_ACTIVE)

windowIsIconified :: IO Bool
windowIsIconified =
    fromC `fmap` glfwGetWindowParam (#const GLFW_ICONIFIED)

windowIsResizable :: IO Bool
windowIsResizable =
    (not . fromC) `fmap` glfwGetWindowParam (#const GLFW_WINDOW_NO_RESIZE)

windowIsHardwareAccelerated :: IO Bool
windowIsHardwareAccelerated =
    fromC `fmap` glfwGetWindowParam (#const GLFW_ACCELERATED)

windowSupportsStereoRendering :: IO Bool
windowSupportsStereoRendering =
    fromC `fmap` glfwGetWindowParam (#const GLFW_STEREO)

getWindowRefreshRate :: IO Int
getWindowRefreshRate =
    fromC `fmap` glfwGetWindowParam (#const GLFW_REFRESH_RATE)

getWindowDimensions :: IO (Int, Int)
getWindowDimensions =
    alloca $ \wp ->
    alloca $ \hp -> do
        glfwGetWindowSize wp hp
        w <- peek wp
        h <- peek hp
        return (fromC w, fromC h)

getWindowValue :: WindowValue -> IO Int
getWindowValue wn =
    fromC `fmap` glfwGetWindowParam (toC wn)

setWindowCloseCallback :: WindowCloseCallback -> IO ()
setWindowCloseCallback cb = do
    ccb <- wrapWindowCloseCallback (toC `fmap` cb)
    glfwSetWindowCloseCallback ccb
    storeCallback windowCloseCallback ccb

setWindowResizeCallback :: WindowResizeCallback -> IO ()
setWindowResizeCallback cb = do
    ccb <- wrapWindowResizeCallback (\w h -> cb (fromC w) (fromC h))
    glfwSetWindowSizeCallback ccb
    storeCallback windowResizeCallback ccb

setWindowRefreshCallback :: WindowRefreshCallback -> IO ()
setWindowRefreshCallback cb = do
    ccb <- wrapWindowRefreshCallback cb
    glfwSetWindowRefreshCallback ccb
    storeCallback windowRefreshCallback ccb

-- -- -- -- -- -- -- -- -- --

data WindowValue
  = NumRedBits
  | NumGreenBits
  | NumBlueBits
  | NumAlphaBits
  | NumDepthBits
  | NumStencilBits
  | NumAccumRedBits
  | NumAccumGreenBits
  | NumAccumBlueBits
  | NumAccumAlphaBits
  | NumAuxBuffers
  | NumFsaaSamples
  deriving (Bounded, Enum, Eq, Show)

instance C WindowValue CInt where
  toC wn = case wn of
      NumRedBits        -> #const GLFW_RED_BITS
      NumGreenBits      -> #const GLFW_GREEN_BITS
      NumBlueBits       -> #const GLFW_BLUE_BITS
      NumAlphaBits      -> #const GLFW_ALPHA_BITS
      NumDepthBits      -> #const GLFW_DEPTH_BITS
      NumStencilBits    -> #const GLFW_STENCIL_BITS
      NumAccumRedBits   -> #const GLFW_ACCUM_RED_BITS
      NumAccumGreenBits -> #const GLFW_ACCUM_GREEN_BITS
      NumAccumBlueBits  -> #const GLFW_ACCUM_BLUE_BITS
      NumAccumAlphaBits -> #const GLFW_ACCUM_ALPHA_BITS
      NumAuxBuffers     -> #const GLFW_AUX_BUFFERS
      NumFsaaSamples    -> #const GLFW_FSAA_SAMPLES

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Input

pollEvents :: IO ()
pollEvents =
    glfwPollEvents

waitEvents :: IO ()
waitEvents =
    glfwWaitEvents

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Keyboard

keyboardKeyIsPressed :: KeyboardKey -> IO Bool
keyboardKeyIsPressed k =
    fromC `fmap` glfwGetKey (toC k)

setKeyboardKeyCallback :: KeyboardKeyCallback -> IO ()
setKeyboardKeyCallback cb = do
    ccb <- wrapKeyboardKeyCallback (\k b -> cb (fromC k) (fromC b))
    glfwSetKeyCallback ccb
    storeCallback keyboardKeyCallback ccb

setKeyboardCharCallback :: KeyboardCharCallback -> IO ()
setKeyboardCharCallback cb = do
    ccb <- wrapKeyboardCharCallback (\c b -> cb (fromC c) (fromC b))
    glfwSetCharCallback ccb
    storeCallback keyboardCharCallback ccb

-- -- -- -- -- -- -- -- -- --

data KeyboardKey
  = KeyboardKeyUnknown
  | KeyboardKeySpace
  | KeyboardKeySpecial
  | KeyboardKeyEsc
  | KeyboardKeyF1
  | KeyboardKeyF2
  | KeyboardKeyF3
  | KeyboardKeyF4
  | KeyboardKeyF5
  | KeyboardKeyF6
  | KeyboardKeyF7
  | KeyboardKeyF8
  | KeyboardKeyF9
  | KeyboardKeyF10
  | KeyboardKeyF11
  | KeyboardKeyF12
  | KeyboardKeyF13
  | KeyboardKeyF14
  | KeyboardKeyF15
  | KeyboardKeyF16
  | KeyboardKeyF17
  | KeyboardKeyF18
  | KeyboardKeyF19
  | KeyboardKeyF20
  | KeyboardKeyF21
  | KeyboardKeyF22
  | KeyboardKeyF23
  | KeyboardKeyF24
  | KeyboardKeyF25
  | KeyboardKeyUp
  | KeyboardKeyDown
  | KeyboardKeyLeft
  | KeyboardKeyRight
  | KeyboardKeyLeftShift
  | KeyboardKeyRightShift
  | KeyboardKeyLeftCtrl
  | KeyboardKeyRightCtrl
  | KeyboardKeyLeftAlt
  | KeyboardKeyRightAlt
  | KeyboardKeyTab
  | KeyboardKeyEnter
  | KeyboardKeyBackspace
  | KeyboardKeyInsert
  | KeyboardKeyDel
  | KeyboardKeyPageup
  | KeyboardKeyPagedown
  | KeyboardKeyHome
  | KeyboardKeyEnd
  | KeyboardKeyKP_0
  | KeyboardKeyKP_1
  | KeyboardKeyKP_2
  | KeyboardKeyKP_3
  | KeyboardKeyKP_4
  | KeyboardKeyKP_5
  | KeyboardKeyKP_6
  | KeyboardKeyKP_7
  | KeyboardKeyKP_8
  | KeyboardKeyKP_9
  | KeyboardKeyKP_Divide
  | KeyboardKeyKP_Multiply
  | KeyboardKeyKP_Subtract
  | KeyboardKeyKP_Add
  | KeyboardKeyKP_Decimal
  | KeyboardKeyKP_Equal
  | KeyboardKeyKP_Enter
  deriving (Bounded, Enum, Eq, Show)

instance C KeyboardKey CInt where
  toC k = case k of
      KeyboardKeyUnknown     -> #const GLFW_KEY_UNKNOWN
      KeyboardKeySpace       -> #const GLFW_KEY_SPACE
      KeyboardKeySpecial     -> #const GLFW_KEY_SPECIAL
      KeyboardKeyEsc         -> #const GLFW_KEY_ESC
      KeyboardKeyF1          -> #const GLFW_KEY_F1
      KeyboardKeyF2          -> #const GLFW_KEY_F2
      KeyboardKeyF3          -> #const GLFW_KEY_F3
      KeyboardKeyF4          -> #const GLFW_KEY_F4
      KeyboardKeyF5          -> #const GLFW_KEY_F5
      KeyboardKeyF6          -> #const GLFW_KEY_F6
      KeyboardKeyF7          -> #const GLFW_KEY_F7
      KeyboardKeyF8          -> #const GLFW_KEY_F8
      KeyboardKeyF9          -> #const GLFW_KEY_F9
      KeyboardKeyF10         -> #const GLFW_KEY_F10
      KeyboardKeyF11         -> #const GLFW_KEY_F11
      KeyboardKeyF12         -> #const GLFW_KEY_F12
      KeyboardKeyF13         -> #const GLFW_KEY_F13
      KeyboardKeyF14         -> #const GLFW_KEY_F14
      KeyboardKeyF15         -> #const GLFW_KEY_F15
      KeyboardKeyF16         -> #const GLFW_KEY_F16
      KeyboardKeyF17         -> #const GLFW_KEY_F17
      KeyboardKeyF18         -> #const GLFW_KEY_F18
      KeyboardKeyF19         -> #const GLFW_KEY_F19
      KeyboardKeyF20         -> #const GLFW_KEY_F20
      KeyboardKeyF21         -> #const GLFW_KEY_F21
      KeyboardKeyF22         -> #const GLFW_KEY_F22
      KeyboardKeyF23         -> #const GLFW_KEY_F23
      KeyboardKeyF24         -> #const GLFW_KEY_F24
      KeyboardKeyF25         -> #const GLFW_KEY_F25
      KeyboardKeyUp          -> #const GLFW_KEY_UP
      KeyboardKeyDown        -> #const GLFW_KEY_DOWN
      KeyboardKeyLeft        -> #const GLFW_KEY_LEFT
      KeyboardKeyRight       -> #const GLFW_KEY_RIGHT
      KeyboardKeyLeftShift   -> #const GLFW_KEY_LSHIFT
      KeyboardKeyRightShift  -> #const GLFW_KEY_RSHIFT
      KeyboardKeyLeftCtrl    -> #const GLFW_KEY_LCTRL
      KeyboardKeyRightCtrl   -> #const GLFW_KEY_RCTRL
      KeyboardKeyLeftAlt     -> #const GLFW_KEY_LALT
      KeyboardKeyRightAlt    -> #const GLFW_KEY_RALT
      KeyboardKeyTab         -> #const GLFW_KEY_TAB
      KeyboardKeyEnter       -> #const GLFW_KEY_ENTER
      KeyboardKeyBackspace   -> #const GLFW_KEY_BACKSPACE
      KeyboardKeyInsert      -> #const GLFW_KEY_INSERT
      KeyboardKeyDel         -> #const GLFW_KEY_DEL
      KeyboardKeyPageup      -> #const GLFW_KEY_PAGEUP
      KeyboardKeyPagedown    -> #const GLFW_KEY_PAGEDOWN
      KeyboardKeyHome        -> #const GLFW_KEY_HOME
      KeyboardKeyEnd         -> #const GLFW_KEY_END
      KeyboardKeyKP_0        -> #const GLFW_KEY_KP_0
      KeyboardKeyKP_1        -> #const GLFW_KEY_KP_1
      KeyboardKeyKP_2        -> #const GLFW_KEY_KP_2
      KeyboardKeyKP_3        -> #const GLFW_KEY_KP_3
      KeyboardKeyKP_4        -> #const GLFW_KEY_KP_4
      KeyboardKeyKP_5        -> #const GLFW_KEY_KP_5
      KeyboardKeyKP_6        -> #const GLFW_KEY_KP_6
      KeyboardKeyKP_7        -> #const GLFW_KEY_KP_7
      KeyboardKeyKP_8        -> #const GLFW_KEY_KP_8
      KeyboardKeyKP_9        -> #const GLFW_KEY_KP_9
      KeyboardKeyKP_Divide   -> #const GLFW_KEY_KP_DIVIDE
      KeyboardKeyKP_Multiply -> #const GLFW_KEY_KP_MULTIPLY
      KeyboardKeyKP_Subtract -> #const GLFW_KEY_KP_SUBTRACT
      KeyboardKeyKP_Add      -> #const GLFW_KEY_KP_ADD
      KeyboardKeyKP_Decimal  -> #const GLFW_KEY_KP_DECIMAL
      KeyboardKeyKP_Equal    -> #const GLFW_KEY_KP_EQUAL
      KeyboardKeyKP_Enter    -> #const GLFW_KEY_KP_ENTER

  fromC i = case i of
      (#const GLFW_KEY_UNKNOWN    ) -> KeyboardKeyUnknown
      (#const GLFW_KEY_SPACE      ) -> KeyboardKeySpace
      (#const GLFW_KEY_SPECIAL    ) -> KeyboardKeySpecial
      (#const GLFW_KEY_ESC        ) -> KeyboardKeyEsc
      (#const GLFW_KEY_F1         ) -> KeyboardKeyF1
      (#const GLFW_KEY_F2         ) -> KeyboardKeyF2
      (#const GLFW_KEY_F3         ) -> KeyboardKeyF3
      (#const GLFW_KEY_F4         ) -> KeyboardKeyF4
      (#const GLFW_KEY_F5         ) -> KeyboardKeyF5
      (#const GLFW_KEY_F6         ) -> KeyboardKeyF6
      (#const GLFW_KEY_F7         ) -> KeyboardKeyF7
      (#const GLFW_KEY_F8         ) -> KeyboardKeyF8
      (#const GLFW_KEY_F9         ) -> KeyboardKeyF9
      (#const GLFW_KEY_F10        ) -> KeyboardKeyF10
      (#const GLFW_KEY_F11        ) -> KeyboardKeyF11
      (#const GLFW_KEY_F12        ) -> KeyboardKeyF12
      (#const GLFW_KEY_F13        ) -> KeyboardKeyF13
      (#const GLFW_KEY_F14        ) -> KeyboardKeyF14
      (#const GLFW_KEY_F15        ) -> KeyboardKeyF15
      (#const GLFW_KEY_F16        ) -> KeyboardKeyF16
      (#const GLFW_KEY_F17        ) -> KeyboardKeyF17
      (#const GLFW_KEY_F18        ) -> KeyboardKeyF18
      (#const GLFW_KEY_F19        ) -> KeyboardKeyF19
      (#const GLFW_KEY_F20        ) -> KeyboardKeyF20
      (#const GLFW_KEY_F21        ) -> KeyboardKeyF21
      (#const GLFW_KEY_F22        ) -> KeyboardKeyF22
      (#const GLFW_KEY_F23        ) -> KeyboardKeyF23
      (#const GLFW_KEY_F24        ) -> KeyboardKeyF24
      (#const GLFW_KEY_F25        ) -> KeyboardKeyF25
      (#const GLFW_KEY_UP         ) -> KeyboardKeyUp
      (#const GLFW_KEY_DOWN       ) -> KeyboardKeyDown
      (#const GLFW_KEY_LEFT       ) -> KeyboardKeyLeft
      (#const GLFW_KEY_RIGHT      ) -> KeyboardKeyRight
      (#const GLFW_KEY_LSHIFT     ) -> KeyboardKeyLeftShift
      (#const GLFW_KEY_RSHIFT     ) -> KeyboardKeyRightShift
      (#const GLFW_KEY_LCTRL      ) -> KeyboardKeyLeftCtrl
      (#const GLFW_KEY_RCTRL      ) -> KeyboardKeyRightCtrl
      (#const GLFW_KEY_LALT       ) -> KeyboardKeyLeftAlt
      (#const GLFW_KEY_RALT       ) -> KeyboardKeyRightAlt
      (#const GLFW_KEY_TAB        ) -> KeyboardKeyTab
      (#const GLFW_KEY_ENTER      ) -> KeyboardKeyEnter
      (#const GLFW_KEY_BACKSPACE  ) -> KeyboardKeyBackspace
      (#const GLFW_KEY_INSERT     ) -> KeyboardKeyInsert
      (#const GLFW_KEY_DEL        ) -> KeyboardKeyDel
      (#const GLFW_KEY_PAGEUP     ) -> KeyboardKeyPageup
      (#const GLFW_KEY_PAGEDOWN   ) -> KeyboardKeyPagedown
      (#const GLFW_KEY_HOME       ) -> KeyboardKeyHome
      (#const GLFW_KEY_END        ) -> KeyboardKeyEnd
      (#const GLFW_KEY_KP_0       ) -> KeyboardKeyKP_0
      (#const GLFW_KEY_KP_1       ) -> KeyboardKeyKP_1
      (#const GLFW_KEY_KP_2       ) -> KeyboardKeyKP_2
      (#const GLFW_KEY_KP_3       ) -> KeyboardKeyKP_3
      (#const GLFW_KEY_KP_4       ) -> KeyboardKeyKP_4
      (#const GLFW_KEY_KP_5       ) -> KeyboardKeyKP_5
      (#const GLFW_KEY_KP_6       ) -> KeyboardKeyKP_6
      (#const GLFW_KEY_KP_7       ) -> KeyboardKeyKP_7
      (#const GLFW_KEY_KP_8       ) -> KeyboardKeyKP_8
      (#const GLFW_KEY_KP_9       ) -> KeyboardKeyKP_9
      (#const GLFW_KEY_KP_DIVIDE  ) -> KeyboardKeyKP_Divide
      (#const GLFW_KEY_KP_MULTIPLY) -> KeyboardKeyKP_Multiply
      (#const GLFW_KEY_KP_SUBTRACT) -> KeyboardKeyKP_Subtract
      (#const GLFW_KEY_KP_ADD     ) -> KeyboardKeyKP_Add
      (#const GLFW_KEY_KP_DECIMAL ) -> KeyboardKeyKP_Decimal
      (#const GLFW_KEY_KP_EQUAL   ) -> KeyboardKeyKP_Equal
      (#const GLFW_KEY_KP_ENTER   ) -> KeyboardKeyKP_Enter
      _                             -> KeyboardKeyUnknown

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Mouse

mouseButtonIsPressed :: MouseButton -> IO Bool
mouseButtonIsPressed mb =
    fromC `fmap` glfwGetMouseButton (toC mb)

getMousePosition :: IO (Int, Int)
getMousePosition =
    alloca $ \px ->
    alloca $ \py -> do
        glfwGetMousePos px py
        x <- peek px
        y <- peek py
        return (fromC x, fromC y)

getMouseWheel :: IO Int
getMouseWheel =
    fromC `fmap` glfwGetMouseWheel

setMousePosition :: Int -> Int -> IO ()
setMousePosition x y =
    glfwSetMousePos (toC x) (toC y)

setMouseWheel :: Int -> IO ()
setMouseWheel =
    glfwSetMouseWheel . toC

setMouseButtonCallback :: MouseButtonCallback -> IO ()
setMouseButtonCallback cb = do
    ccb <- wrapMouseButtonCallback (\b p -> cb (fromC b) (fromC p))
    glfwSetMouseButtonCallback ccb
    storeCallback mouseButtonCallback ccb

setMousePositionCallback :: MousePositionCallback -> IO ()
setMousePositionCallback cb = do
    ccb <- wrapMousePositionCallback (\x y -> cb (fromC x) (fromC y))
    glfwSetMousePosCallback ccb
    storeCallback mousePositionCallback ccb

setMouseWheelCallback :: MouseWheelCallback -> IO ()
setMouseWheelCallback cb = do
    ccb <- wrapMouseWheelCallback (cb . fromC)
    glfwSetMouseWheelCallback ccb
    storeCallback mouseWheelCallback ccb

-- -- -- -- -- -- -- -- -- --

data MouseButton
  = MouseButton0 | MouseButton1 | MouseButton2 | MouseButton3
  | MouseButton4 | MouseButton5 | MouseButton6 | MouseButton7
  deriving (Bounded, Enum, Eq, Show)

instance C MouseButton CInt where
  toC mb = case mb of
      MouseButton0 -> #const GLFW_MOUSE_BUTTON_1
      MouseButton1 -> #const GLFW_MOUSE_BUTTON_2
      MouseButton2 -> #const GLFW_MOUSE_BUTTON_3
      MouseButton3 -> #const GLFW_MOUSE_BUTTON_4
      MouseButton4 -> #const GLFW_MOUSE_BUTTON_5
      MouseButton5 -> #const GLFW_MOUSE_BUTTON_6
      MouseButton6 -> #const GLFW_MOUSE_BUTTON_7
      MouseButton7 -> #const GLFW_MOUSE_BUTTON_8

  fromC i = case i of
      (#const GLFW_MOUSE_BUTTON_1) -> MouseButton0
      (#const GLFW_MOUSE_BUTTON_2) -> MouseButton1
      (#const GLFW_MOUSE_BUTTON_3) -> MouseButton2
      (#const GLFW_MOUSE_BUTTON_4) -> MouseButton3
      (#const GLFW_MOUSE_BUTTON_5) -> MouseButton4
      (#const GLFW_MOUSE_BUTTON_6) -> MouseButton5
      (#const GLFW_MOUSE_BUTTON_7) -> MouseButton6
      (#const GLFW_MOUSE_BUTTON_8) -> MouseButton7
      _                            -> makeFromCError "MouseButton" i

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Joystick

joystickIsPresent :: Joystick -> IO Bool
joystickIsPresent j =
    fromC `fmap` glfwGetJoystickParam (toC j) (#const GLFW_PRESENT)

getNumJoystickAxes :: Joystick -> IO Int
getNumJoystickAxes j =
    fromC `fmap` glfwGetJoystickParam (toC j) (#const GLFW_AXES)

getNumJoystickButtons :: Joystick -> IO Int
getNumJoystickButtons j =
    fromC `fmap` glfwGetJoystickParam (toC j) (#const GLFW_BUTTONS)

getJoystickPosition :: Joystick -> Int -> IO [Float]
getJoystickPosition j m =
    if m < 1
      then return []
      else allocaArray m $ \ptr -> do
               n <- fromC `fmap` glfwGetJoystickPos (toC j) ptr (toC m)
               a <- peekArray n ptr
               return $ map fromC a

joystickButtonsArePressed :: Joystick -> Int -> IO [Bool]
joystickButtonsArePressed j m =
    if m < 1
      then return []
      else allocaArray m $ \ptr -> do
               n <- fromC `fmap` glfwGetJoystickButtons (toC j) ptr (toC m)
               a <- peekArray n ptr :: IO [CUChar]
               return $ map ((glfwPress ==) . fromIntegral) a

-- -- -- -- -- -- -- -- -- --

data Joystick
  = Joystick0  | Joystick1  | Joystick2  | Joystick3
  | Joystick4  | Joystick5  | Joystick6  | Joystick7
  | Joystick8  | Joystick9  | Joystick10 | Joystick11
  | Joystick12 | Joystick13 | Joystick14 | Joystick15
  deriving (Bounded, Enum, Eq, Show)

instance C Joystick CInt where
  toC j = case j of
      Joystick0  -> #const GLFW_JOYSTICK_1
      Joystick1  -> #const GLFW_JOYSTICK_2
      Joystick2  -> #const GLFW_JOYSTICK_3
      Joystick3  -> #const GLFW_JOYSTICK_4
      Joystick4  -> #const GLFW_JOYSTICK_5
      Joystick5  -> #const GLFW_JOYSTICK_6
      Joystick6  -> #const GLFW_JOYSTICK_7
      Joystick7  -> #const GLFW_JOYSTICK_8
      Joystick8  -> #const GLFW_JOYSTICK_9
      Joystick9  -> #const GLFW_JOYSTICK_10
      Joystick10 -> #const GLFW_JOYSTICK_11
      Joystick11 -> #const GLFW_JOYSTICK_12
      Joystick12 -> #const GLFW_JOYSTICK_13
      Joystick13 -> #const GLFW_JOYSTICK_14
      Joystick14 -> #const GLFW_JOYSTICK_15
      Joystick15 -> #const GLFW_JOYSTICK_16

  fromC i = case i of
      (#const GLFW_JOYSTICK_1 ) -> Joystick0
      (#const GLFW_JOYSTICK_2 ) -> Joystick1
      (#const GLFW_JOYSTICK_3 ) -> Joystick2
      (#const GLFW_JOYSTICK_4 ) -> Joystick3
      (#const GLFW_JOYSTICK_5 ) -> Joystick4
      (#const GLFW_JOYSTICK_6 ) -> Joystick5
      (#const GLFW_JOYSTICK_7 ) -> Joystick6
      (#const GLFW_JOYSTICK_8 ) -> Joystick7
      (#const GLFW_JOYSTICK_9 ) -> Joystick8
      (#const GLFW_JOYSTICK_10) -> Joystick9
      (#const GLFW_JOYSTICK_11) -> Joystick10
      (#const GLFW_JOYSTICK_12) -> Joystick11
      (#const GLFW_JOYSTICK_13) -> Joystick12
      (#const GLFW_JOYSTICK_14) -> Joystick13
      (#const GLFW_JOYSTICK_15) -> Joystick14
      (#const GLFW_JOYSTICK_16) -> Joystick15
      _                         -> makeFromCError "Joystick" i

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Time

getTime :: IO Double
getTime =
    realToFrac `fmap` glfwGetTime

setTime :: Double -> IO ()
setTime =
    glfwSetTime . realToFrac

resetTime :: IO ()
resetTime =
    setTime (0 :: Double)

sleep :: Double -> IO ()
sleep =
    glfwSleep . realToFrac

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- Version information

getGlfwVersion :: IO Version
getGlfwVersion =
    alloca $ \p0 ->
    alloca $ \p1 ->
    alloca $ \p2 -> do
        glfwGetVersion p0 p1 p2
        v0 <- fromC `fmap` peek p0
        v1 <- fromC `fmap` peek p1
        v2 <- fromC `fmap` peek p2
        return $ Version [v0, v1, v2] []

getGlVersion :: IO Version
getGlVersion =
    alloca $ \p0 ->
    alloca $ \p1 ->
    alloca $ \p2 -> do
        glfwGetGLVersion p0 p1 p2
        v0 <- fromC `fmap` peek p0
        v1 <- fromC `fmap` peek p1
        v2 <- fromC `fmap` peek p2
        return $ Version [v0, v1, v2] []

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

glfwPress :: CInt
glfwPress = #const GLFW_PRESS

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class C h c where
  toC   :: h -> c
  fromC :: c -> h

  toC   = undefined
  fromC = undefined

makeFromCError :: (Show c) => String -> c -> a
makeFromCError s c = error (s ++ " fromC: no match for " ++ show c)

-- -- -- -- -- -- -- -- -- --

instance C Bool CInt where
  toC False = #const GL_FALSE
  toC True  = #const GL_TRUE

  fromC (#const GL_FALSE) = False
  fromC (#const GL_TRUE)  = True
  fromC i                 = makeFromCError "Bool" i

-- -- -- -- -- -- -- -- -- --

instance C Char CInt where
  toC   = fromIntegral . ord
  fromC = chr . fromIntegral

-- -- -- -- -- -- -- -- -- --

instance C Float CFloat where
  toC   = realToFrac
  fromC = realToFrac

-- -- -- -- -- -- -- -- -- --

instance C Int CInt where
  toC   = fromIntegral
  fromC = fromIntegral

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

keyboardCharCallback  :: IORef (Maybe (FunPtr GlfwKeyboardCharCallback))
keyboardKeyCallback   :: IORef (Maybe (FunPtr GlfwKeyboardKeyCallback))
mouseButtonCallback   :: IORef (Maybe (FunPtr GlfwMouseButtonCallback))
mousePositionCallback :: IORef (Maybe (FunPtr GlfwMousePositionCallback))
mouseWheelCallback    :: IORef (Maybe (FunPtr GlfwMouseWheelCallback))
windowCloseCallback   :: IORef (Maybe (FunPtr GlfwWindowCloseCallback))
windowRefreshCallback :: IORef (Maybe (FunPtr GlfwWindowRefreshCallback))
windowResizeCallback  :: IORef (Maybe (FunPtr GlfwWindowResizeCallback))

keyboardCharCallback  = unsafePerformIO (newIORef Nothing)
keyboardKeyCallback   = unsafePerformIO (newIORef Nothing)
mouseButtonCallback   = unsafePerformIO (newIORef Nothing)
mousePositionCallback = unsafePerformIO (newIORef Nothing)
mouseWheelCallback    = unsafePerformIO (newIORef Nothing)
windowCloseCallback   = unsafePerformIO (newIORef Nothing)
windowRefreshCallback = unsafePerformIO (newIORef Nothing)
windowResizeCallback  = unsafePerformIO (newIORef Nothing)

storeCallback :: IORef (Maybe (FunPtr a)) -> FunPtr a -> IO ()
storeCallback ior cb =
    atomicModifyIORef ior (\mcb -> (Just cb, mcb)) >>= maybe (return ()) freeHaskellFunPtr
