{-# LANGUAGE DeriveDataTypeable #-}

module Graphics.UI.GLFW.Types where

--------------------------------------------------------------------------------

import Data.Data     (Data)
import Data.Typeable (Typeable)
import Foreign.Ptr   (Ptr)

import Bindings.GLFW

--------------------------------------------------------------------------------
-- Error handling

data Error =
    Error'NotInitialized
  | Error'NoCurrentContext
  | Error'InvalidEnum
  | Error'InvalidValue
  | Error'OutOfMemory
  | Error'ApiUnavailable
  | Error'VersionUnavailable
  | Error'PlatformError
  | Error'FormatUnavailable
  deriving (Data, Eq, Show, Typeable)

--------------------------------------------------------------------------------
-- Initialization and version information

data Version = Version
  { versionMajor    :: Int
  , versionMinor    :: Int
  , versionRevision :: Int
  } deriving (Data, Eq, Show, Typeable)

--------------------------------------------------------------------------------
-- Monitor handling

newtype Monitor = Monitor
  { unMonitor :: Ptr C'GLFWmonitor
  } deriving (Data, Eq, Show, Typeable)

data MonitorState =
    MonitorState'Connected
  | MonitorState'Disconnected
  deriving (Data, Eq, Show, Typeable)

data VideoMode = VideoMode
  { videoModeWidth       :: Int
  , videoModeHeight      :: Int
  , videoModeRedBits     :: Int
  , videoModeGreenBits   :: Int
  , videoModeBlueBits    :: Int
  , videoModeRefreshRate :: Int
  } deriving (Data, Eq, Show, Typeable)

-- It would be bad to give clients a way to construct invalid gamma ramps with
-- lists of unequal length, so this constructor should not be exported.
data GammaRamp = GammaRamp
  { gammaRampRed   :: [Int]
  , gammaRampGreen :: [Int]
  , gammaRampBlue  :: [Int]
  } deriving (Data, Eq, Show, Typeable)

-- Smart constructor for GammaRamp.
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

newtype Window = Window
  { unWindow :: Ptr C'GLFWwindow
  } deriving (Data, Eq, Show, Typeable)

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
  deriving (Data, Eq, Show, Typeable)

data FocusState =
    FocusState'Focused
  | FocusState'Defocused
  deriving (Data, Eq, Show, Typeable)

data IconifyState =
    IconifyState'Iconified
  | IconifyState'NotIconified
  deriving (Data, Eq, Show, Typeable)

data ContextRobustness =
    ContextRobustness'NoRobustness
  | ContextRobustness'NoResetNotification
  | ContextRobustness'LoseContextOnReset
  deriving (Data, Eq, Show, Typeable)

data OpenGLProfile =
    OpenGLProfile'Any
  | OpenGLProfile'Compat
  | OpenGLProfile'Core
  deriving (Data, Eq, Show, Typeable)

data ClientAPI =
    ClientAPI'OpenGL
  | ClientAPI'OpenGLES
  deriving (Data, Eq, Show, Typeable)

--------------------------------------------------------------------------------
-- Input handling

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
  deriving (Data, Eq, Show, Typeable)

data KeyState =
    KeyState'Pressed
  | KeyState'Released
  | KeyState'Repeating
  deriving (Data, Eq, Show, Typeable)

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
  deriving (Data, Eq, Ord, Show, Typeable)

data JoystickButtonState =
    JoystickButtonState'Pressed
  | JoystickButtonState'Released
  deriving (Data, Eq, Show, Typeable)

data MouseButton =
    MouseButton'1
  | MouseButton'2
  | MouseButton'3
  | MouseButton'4
  | MouseButton'5
  | MouseButton'6
  | MouseButton'7
  | MouseButton'8
  deriving (Data, Eq, Show, Typeable)

data MouseButtonState =
    MouseButtonState'Pressed
  | MouseButtonState'Released
  deriving (Data, Eq, Show, Typeable)

data CursorState =
    CursorState'InWindow
  | CursorState'NotInWindow
  deriving (Data, Eq, Show, Typeable)

data CursorInputMode =
    CursorInputMode'Normal
  | CursorInputMode'Hidden
  | CursorInputMode'Disabled
  deriving (Data, Eq, Show, Typeable)

data StickyKeysInputMode =
    StickyKeysInputMode'Enabled
  | StickyKeysInputMode'Disabled
  deriving (Data, Eq, Show, Typeable)

data StickyMouseButtonsInputMode =
    StickyMouseButtonsInputMode'Enabled
  | StickyMouseButtonsInputMode'Disabled
  deriving (Data, Eq, Show, Typeable)

data ModifierKeys = ModifierKeys
  { modifierKeysShift   :: Bool
  , modifierKeysControl :: Bool
  , modifierKeysAlt     :: Bool
  , modifierKeysSuper   :: Bool
  } deriving (Data, Eq, Show, Typeable)

--------------------------------------------------------------------------------

{-# ANN module "HLint: ignore Use camelCase" #-}
