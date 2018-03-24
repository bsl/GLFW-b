{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | The types of the package. This module is considered "internal", and the
-- types are re-exported from Graphics.UI.GLFW as necessary.
module Graphics.UI.GLFW.Types where

--------------------------------------------------------------------------------

import Control.DeepSeq  (NFData)
import Data.Data        (Data)
import Data.IORef       (IORef)
import Data.Typeable    (Typeable)
import Foreign.Ptr      (Ptr)
import Foreign.C.Types  (CUChar(..))
import GHC.Generics

import Bindings.GLFW

--------------------------------------------------------------------------------
-- Error handling

-- | An enum for one of the <http://www.glfw.org/docs/3.1/group__errors.html#ga196e125ef261d94184e2b55c05762f14 GLFW error codes>.
data Error =
    Error'NotInitialized -- ^ <http://www.glfw.org/docs/3.1/group__errors.html#ga2374ee02c177f12e1fa76ff3ed15e14a doc>
  | Error'NoCurrentContext -- ^ <http://www.glfw.org/docs/3.1/group__errors.html#gaa8290386e9528ccb9e42a3a4e16fc0d0 doc>
  | Error'InvalidEnum -- ^ <http://www.glfw.org/docs/3.1/group__errors.html#ga76f6bb9c4eea73db675f096b404593ce doc>
  | Error'InvalidValue -- ^ <http://www.glfw.org/docs/3.1/group__errors.html#gaaf2ef9aa8202c2b82ac2d921e554c687 doc>
  | Error'OutOfMemory -- ^ <http://www.glfw.org/docs/3.1/group__errors.html#ga9023953a2bcb98c2906afd071d21ee7f doc>
  | Error'ApiUnavailable -- ^ <http://www.glfw.org/docs/3.1/group__errors.html#ga56882b290db23261cc6c053c40c2d08e doc>
  | Error'VersionUnavailable -- ^ <http://www.glfw.org/docs/3.1/group__errors.html#gad16c5565b4a69f9c2a9ac2c0dbc89462 doc>
  | Error'PlatformError -- ^ <http://www.glfw.org/docs/3.1/group__errors.html#gad44162d78100ea5e87cdd38426b8c7a1 doc>
  | Error'FormatUnavailable -- ^ <http://www.glfw.org/docs/3.1/group__errors.html#ga196e125ef261d94184e2b55c05762f14 doc>
  deriving (Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData Error

--------------------------------------------------------------------------------
-- Initialization and version information

-- | The library version of the GLFW implementation in use.
-- See <http://www.glfw.org/docs/3.1/intro.html#intro_version Version Management>
data Version = Version
  { versionMajor    :: Int
  , versionMinor    :: Int
  , versionRevision :: Int
  } deriving (Data, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData Version

--------------------------------------------------------------------------------
-- Monitor handling

-- | Represents a physical monitor that's currently connected.
-- See the <http://www.glfw.org/docs/3.1/monitor.html Monitor Guide>
newtype Monitor = Monitor
  { unMonitor :: Ptr C'GLFWmonitor
  } deriving (Data, Eq, Ord, Show, Typeable, Generic)

-- | Part of the 'MonitorCallback', for when a monitor gets connected or disconnected.
data MonitorState =
    MonitorState'Connected
  | MonitorState'Disconnected
  deriving (Data, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData MonitorState

-- | See <http://www.glfw.org/docs/3.1/monitor.html#monitor_modes Video Modes>
data VideoMode = VideoMode
  { videoModeWidth       :: Int
  , videoModeHeight      :: Int
  , videoModeRedBits     :: Int
  , videoModeGreenBits   :: Int
  , videoModeBlueBits    :: Int
  , videoModeRefreshRate :: Int
  } deriving (Data, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData VideoMode

-- | Lets you adjust the gamma of a monitor. To ensure that only valid values are created, use 'makeGammaRamp'.
-- See <http://www.glfw.org/docs/3.1/monitor.html#monitor_gamma Gamma Ramp>.
data GammaRamp = GammaRamp
  -- NOTE: It would be bad to give clients a way to construct invalid gamma ramps
  -- with lists of unequal length, so this constructor should not be exported.
  { gammaRampRed   :: [Int]
  , gammaRampGreen :: [Int]
  , gammaRampBlue  :: [Int]
  } deriving (Data, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData GammaRamp

-- | Smart constructor for a 'GammaRamp'.
makeGammaRamp :: [Int] -> [Int] -> [Int] -> Maybe GammaRamp
makeGammaRamp rs gs bs
    | lengthsEqual = Just $ GammaRamp rs gs bs
    | otherwise    = Nothing
  where
    lengthsEqual =
      let rsl = length rs
          gsl = length gs
          bsl = length bs
      in rsl == gsl && gsl == bsl

--------------------------------------------------------------------------------
-- Window handling

-- | Collects all the callbacks that can be associated with a Window into a single place.
data WindowCallbacks = WindowCallbacks
  { storedCharFun             :: IORef C'GLFWcharfun
  , storedCursorEnterFun      :: IORef C'GLFWcursorenterfun
  , storedCursorPosFun        :: IORef C'GLFWcursorposfun
  , storedFramebufferSizeFun  :: IORef C'GLFWframebuffersizefun
  , storedKeyFun              :: IORef C'GLFWkeyfun
  , storedMouseButtonFun      :: IORef C'GLFWmousebuttonfun
  , storedScrollFun           :: IORef C'GLFWscrollfun
  , storedWindowCloseFun      :: IORef C'GLFWwindowclosefun
  , storedWindowFocusFun      :: IORef C'GLFWwindowfocusfun
  , storedWindowIconifyFun    :: IORef C'GLFWwindowiconifyfun
  , storedWindowPosFun        :: IORef C'GLFWwindowposfun
  , storedWindowRefreshFun    :: IORef C'GLFWwindowrefreshfun
  , storedWindowSizeFun       :: IORef C'GLFWwindowsizefun
  , storedDropFun             :: IORef C'GLFWdropfun
  }

-- | Reprisents a GLFW window value.
-- See the <http://www.glfw.org/docs/3.1/window.html Window Guide>
newtype Window = Window
  { unWindow :: Ptr C'GLFWwindow
  } deriving (Data, Eq, Ord, Show, Typeable, Generic)

-- | Lets you set various window hints before creating a 'Window'.
-- See <http://www.glfw.org/docs/3.1/window.html#window_hints Window Hints>,
-- particularly <http://www.glfw.org/docs/3.1/window.html#window_hints_values Supported and Default Values>.
data WindowHint =
    WindowHint'Resizable           Bool
  | WindowHint'Visible             Bool
  | WindowHint'Decorated           Bool
  | WindowHint'RedBits             Int
  | WindowHint'GreenBits           Int
  | WindowHint'BlueBits            Int
  | WindowHint'AlphaBits           Int
  | WindowHint'DepthBits           Int
  | WindowHint'StencilBits         Int
  | WindowHint'AccumRedBits        Int
  | WindowHint'AccumGreenBits      Int
  | WindowHint'AccumBlueBits       Int
  | WindowHint'AccumAlphaBits      Int
  | WindowHint'AuxBuffers          Int
  | WindowHint'Samples             Int
  | WindowHint'RefreshRate         Int
  | WindowHint'Stereo              Bool
  | WindowHint'sRGBCapable         Bool
  | WindowHint'ClientAPI           ClientAPI
  | WindowHint'ContextVersionMajor Int
  | WindowHint'ContextVersionMinor Int
  | WindowHint'ContextRobustness   ContextRobustness
  | WindowHint'OpenGLForwardCompat Bool
  | WindowHint'OpenGLDebugContext  Bool
  | WindowHint'OpenGLProfile       OpenGLProfile
  deriving (Data, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData WindowHint

-- | For use with the focus callback.
data FocusState =
    FocusState'Focused
  | FocusState'Defocused
  deriving (Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData FocusState

-- | For use with the iconify callback. (note: iconified means minimized)
data IconifyState =
    IconifyState'Iconified
  | IconifyState'NotIconified
  deriving (Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData IconifyState

-- | The OpenGL robustness strategy.
data ContextRobustness =
    ContextRobustness'NoRobustness
  | ContextRobustness'NoResetNotification
  | ContextRobustness'LoseContextOnReset
  deriving (Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData ContextRobustness

-- | The OpenGL profile.
data OpenGLProfile =
    OpenGLProfile'Any
  | OpenGLProfile'Compat
  | OpenGLProfile'Core
  deriving (Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData OpenGLProfile

-- | The type of OpenGL to create a context for.
data ClientAPI =
    ClientAPI'OpenGL
  | ClientAPI'OpenGLES
  deriving (Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData ClientAPI

--------------------------------------------------------------------------------
-- Input handling

-- | Part of the <http://www.glfw.org/docs/3.1/input.html#input_keyboard Keyboard Input> system.
data Key =
    Key'Unknown
  | Key'Space
  | Key'Apostrophe
  | Key'Comma
  | Key'Minus
  | Key'Period
  | Key'Slash
  | Key'0
  | Key'1
  | Key'2
  | Key'3
  | Key'4
  | Key'5
  | Key'6
  | Key'7
  | Key'8
  | Key'9
  | Key'Semicolon
  | Key'Equal
  | Key'A
  | Key'B
  | Key'C
  | Key'D
  | Key'E
  | Key'F
  | Key'G
  | Key'H
  | Key'I
  | Key'J
  | Key'K
  | Key'L
  | Key'M
  | Key'N
  | Key'O
  | Key'P
  | Key'Q
  | Key'R
  | Key'S
  | Key'T
  | Key'U
  | Key'V
  | Key'W
  | Key'X
  | Key'Y
  | Key'Z
  | Key'LeftBracket
  | Key'Backslash
  | Key'RightBracket
  | Key'GraveAccent
  | Key'World1
  | Key'World2
  | Key'Escape
  | Key'Enter
  | Key'Tab
  | Key'Backspace
  | Key'Insert
  | Key'Delete
  | Key'Right
  | Key'Left
  | Key'Down
  | Key'Up
  | Key'PageUp
  | Key'PageDown
  | Key'Home
  | Key'End
  | Key'CapsLock
  | Key'ScrollLock
  | Key'NumLock
  | Key'PrintScreen
  | Key'Pause
  | Key'F1
  | Key'F2
  | Key'F3
  | Key'F4
  | Key'F5
  | Key'F6
  | Key'F7
  | Key'F8
  | Key'F9
  | Key'F10
  | Key'F11
  | Key'F12
  | Key'F13
  | Key'F14
  | Key'F15
  | Key'F16
  | Key'F17
  | Key'F18
  | Key'F19
  | Key'F20
  | Key'F21
  | Key'F22
  | Key'F23
  | Key'F24
  | Key'F25
  | Key'Pad0
  | Key'Pad1
  | Key'Pad2
  | Key'Pad3
  | Key'Pad4
  | Key'Pad5
  | Key'Pad6
  | Key'Pad7
  | Key'Pad8
  | Key'Pad9
  | Key'PadDecimal
  | Key'PadDivide
  | Key'PadMultiply
  | Key'PadSubtract
  | Key'PadAdd
  | Key'PadEnter
  | Key'PadEqual
  | Key'LeftShift
  | Key'LeftControl
  | Key'LeftAlt
  | Key'LeftSuper
  | Key'RightShift
  | Key'RightControl
  | Key'RightAlt
  | Key'RightSuper
  | Key'Menu
  deriving (Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData Key

-- | The state of an individual key when 'getKey' is called.
data KeyState =
    KeyState'Pressed
  | KeyState'Released
  | KeyState'Repeating
  deriving (Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData KeyState

-- | For use with the <http://www.glfw.org/docs/3.1/input.html#joystick Joystick Input> system.
data Joystick =
    Joystick'1
  | Joystick'2
  | Joystick'3
  | Joystick'4
  | Joystick'5
  | Joystick'6
  | Joystick'7
  | Joystick'8
  | Joystick'9
  | Joystick'10
  | Joystick'11
  | Joystick'12
  | Joystick'13
  | Joystick'14
  | Joystick'15
  | Joystick'16
  deriving (Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData Joystick

-- | If a given joystick button is pressed or not when 'getJoystickButtons' is called.
data JoystickButtonState =
    JoystickButtonState'Pressed
  | JoystickButtonState'Released
  deriving (Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData JoystickButtonState

-- | Part of the <http://www.glfw.org/docs/3.1/input.html#input_mouse Mouse Input> system.
data MouseButton =
    MouseButton'1
  | MouseButton'2
  | MouseButton'3
  | MouseButton'4
  | MouseButton'5
  | MouseButton'6
  | MouseButton'7
  | MouseButton'8
  deriving (Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData MouseButton

-- | If the mouse button is pressed or not when 'getMouseButton' is called.
data MouseButtonState =
    MouseButtonState'Pressed
  | MouseButtonState'Released
  deriving (Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData MouseButtonState

-- | If the mouse's cursor is in the window or not.
data CursorState =
    CursorState'InWindow
  | CursorState'NotInWindow
  deriving (Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData CursorState

-- | Allows for special forms of mouse input.
-- See <http://www.glfw.org/docs/3.1/input.html#cursor_mode Cursor Modes>
data CursorInputMode =
    CursorInputMode'Normal
  | CursorInputMode'Hidden
  | CursorInputMode'Disabled
  deriving (Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData CursorInputMode

-- | When sticky keys is enabled, once a key is pressed it will remain pressed
-- at least until the state is polled with 'getKey'. After that, if the key has
-- been released it will switch back to released. This helps prevent problems
-- with low-resolution polling missing key pressed. Note that use of the
-- callbacks to avoid this problem the the recommended route, and this is just
-- for a fallback.
data StickyKeysInputMode =
    StickyKeysInputMode'Enabled
  | StickyKeysInputMode'Disabled
  deriving (Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData StickyKeysInputMode

-- | This is the mouse version of "StickyKeysInputMode".
data StickyMouseButtonsInputMode =
    StickyMouseButtonsInputMode'Enabled
  | StickyMouseButtonsInputMode'Disabled
  deriving (Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData StickyMouseButtonsInputMode

-- | Modifier keys that were pressed as part of another keypress event.
data ModifierKeys = ModifierKeys
  { modifierKeysShift   :: Bool
  , modifierKeysControl :: Bool
  , modifierKeysAlt     :: Bool
  , modifierKeysSuper   :: Bool
  } deriving (Data, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData ModifierKeys

--------------------------------------------------------------------------------
-- 3.1 Additions
--------------------------------------------------------------------------------

deriving instance Data CUChar

-- | GLFW image data, for setting up custom mouse cursor appearnaces.
data Image = Image
  { imageWidth  :: Int
  , imageHeight :: Int
  , imagePixels :: [CUChar]
  } deriving (Data, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData Image

-- | Reprisents a GLFW cursor.
newtype Cursor = Cursor
  { unCursor :: Ptr C'GLFWcursor
  } deriving (Data, Eq, Ord, Show, Typeable, Generic)

-- | Lets you use one of the standard cursor appearnaces that the local
-- system theme provides for.
-- See <http://www.glfw.org/docs/3.1/input.html#cursor_standard Standard Cursor Creation>.
data StandardCursorShape =
    StandardCursorShape'Arrow
  | StandardCursorShape'IBeam
  | StandardCursorShape'Crosshair
  | StandardCursorShape'Hand
  | StandardCursorShape'HResize
  | StandardCursorShape'VResize
  deriving (Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData StandardCursorShape

--------------------------------------------------------------------------------

{-# ANN module "HLint: ignore Use camelCase" #-}
