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
import Data.Word        (Word8)
import Foreign.Ptr      (Ptr)
import Foreign.C.Types  (CUChar(..))
import GHC.Generics

import Bindings.GLFW

--------------------------------------------------------------------------------
-- Error handling

-- | An enum for one of the <http://www.glfw.org/docs/3.3/group__errors.html#ga196e125ef261d94184e2b55c05762f14 GLFW error codes>.
data Error =
    Error'NotInitialized -- ^ <http://www.glfw.org/docs/3.3/group__errors.html#ga2374ee02c177f12e1fa76ff3ed15e14a doc>
  | Error'NoCurrentContext -- ^ <http://www.glfw.org/docs/3.3/group__errors.html#gaa8290386e9528ccb9e42a3a4e16fc0d0 doc>
  | Error'InvalidEnum -- ^ <http://www.glfw.org/docs/3.3/group__errors.html#ga76f6bb9c4eea73db675f096b404593ce doc>
  | Error'InvalidValue -- ^ <http://www.glfw.org/docs/3.3/group__errors.html#gaaf2ef9aa8202c2b82ac2d921e554c687 doc>
  | Error'OutOfMemory -- ^ <http://www.glfw.org/docs/3.3/group__errors.html#ga9023953a2bcb98c2906afd071d21ee7f doc>
  | Error'ApiUnavailable -- ^ <http://www.glfw.org/docs/3.3/group__errors.html#ga56882b290db23261cc6c053c40c2d08e doc>
  | Error'VersionUnavailable -- ^ <http://www.glfw.org/docs/3.3/group__errors.html#gad16c5565b4a69f9c2a9ac2c0dbc89462 doc>
  | Error'PlatformError -- ^ <http://www.glfw.org/docs/3.3/group__errors.html#gad44162d78100ea5e87cdd38426b8c7a1 doc>
  | Error'FormatUnavailable -- ^ <http://www.glfw.org/docs/3.3/group__errors.html#ga196e125ef261d94184e2b55c05762f14 doc>
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData Error

--------------------------------------------------------------------------------
-- Initialization and version information

-- | Initialization hints are set before glfwInit and affect how the library
-- behaves until termination. Hints are set with glfwInitHint. See
-- <https://www.glfw.org/docs/3.3/intro_guide.html#init_hints Init Hints>
data InitHint
  = InitHint'JoystickHatButtons
  | InitHint'CocoaChdirResources
  | InitHint'CocoaMenubar
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData InitHint

-- | The library version of the GLFW implementation in use.
-- See <http://www.glfw.org/docs/3.3/intro.html#intro_version Version Management>
data Version = Version
  { versionMajor    :: {-# UNPACK #-} !Int
  , versionMinor    :: {-# UNPACK #-} !Int
  , versionRevision :: {-# UNPACK #-} !Int
  } deriving (Data, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData Version

--------------------------------------------------------------------------------
-- Monitor handling

-- | Represents a physical monitor that's currently connected.
-- See the <http://www.glfw.org/docs/3.3/monitor.html Monitor Guide>
newtype Monitor = Monitor
  { unMonitor :: Ptr C'GLFWmonitor
  } deriving (Data, Eq, Ord, Show, Typeable, Generic)

-- | Part of the t'Graphics.UI.GLFW.MonitorCallback', for when a monitor gets
-- connected or disconnected.
data MonitorState =
    MonitorState'Connected
  | MonitorState'Disconnected
  deriving (Data, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData MonitorState

-- | See <http://www.glfw.org/docs/3.3/monitor.html#monitor_modes Video Modes>
data VideoMode = VideoMode
  { videoModeWidth       :: {-# UNPACK #-} !Int
  , videoModeHeight      :: {-# UNPACK #-} !Int
  , videoModeRedBits     :: {-# UNPACK #-} !Int
  , videoModeGreenBits   :: {-# UNPACK #-} !Int
  , videoModeBlueBits    :: {-# UNPACK #-} !Int
  , videoModeRefreshRate :: {-# UNPACK #-} !Int
  } deriving (Data, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData VideoMode

-- | Lets you adjust the gamma of a monitor. To ensure that only valid values are created, use 'makeGammaRamp'.
-- See <http://www.glfw.org/docs/3.3/monitor.html#monitor_gamma Gamma Ramp>.
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
  { storedCharFun               :: !(IORef C'GLFWcharfun)
  , storedCharModsFun           :: !(IORef C'GLFWcharmodsfun)
  , storedCursorEnterFun        :: !(IORef C'GLFWcursorenterfun)
  , storedCursorPosFun          :: !(IORef C'GLFWcursorposfun)
  , storedFramebufferSizeFun    :: !(IORef C'GLFWframebuffersizefun)
  , storedKeyFun                :: !(IORef C'GLFWkeyfun)
  , storedMouseButtonFun        :: !(IORef C'GLFWmousebuttonfun)
  , storedScrollFun             :: !(IORef C'GLFWscrollfun)
  , storedWindowCloseFun        :: !(IORef C'GLFWwindowclosefun)
  , storedWindowFocusFun        :: !(IORef C'GLFWwindowfocusfun)
  , storedWindowIconifyFun      :: !(IORef C'GLFWwindowiconifyfun)
  , storedWindowPosFun          :: !(IORef C'GLFWwindowposfun)
  , storedWindowRefreshFun      :: !(IORef C'GLFWwindowrefreshfun)
  , storedWindowSizeFun         :: !(IORef C'GLFWwindowsizefun)
  , storedWindowContentScaleFun :: !(IORef C'GLFWwindowcontentscalefun)
  , storedWindowMaximizeFun     :: !(IORef C'GLFWwindowmaximizefun)
  , storedDropFun               :: !(IORef C'GLFWdropfun)
  }

-- | Represents a GLFW window value.
-- See the <http://www.glfw.org/docs/3.3/window.html Window Guide>
newtype Window = Window
  { unWindow :: Ptr C'GLFWwindow
  } deriving (Data, Eq, Ord, Show, Typeable, Generic)

-- | Lets you set various window hints before creating a 'Window'.
-- See <http://www.glfw.org/docs/3.3/window.html#window_hints Window Hints>,
-- particularly <http://www.glfw.org/docs/3.3/window.html#window_hints_values Supported and Default Values>.
data WindowHint =
    WindowHint'Resizable              !Bool
  | WindowHint'Visible                !Bool
  | WindowHint'Decorated              !Bool
  | WindowHint'RedBits                !(Maybe Int)
  | WindowHint'GreenBits              !(Maybe Int)
  | WindowHint'BlueBits               !(Maybe Int)
  | WindowHint'AlphaBits              !(Maybe Int)
  | WindowHint'DepthBits              !(Maybe Int)
  | WindowHint'StencilBits            !(Maybe Int)
  | WindowHint'AccumRedBits           !(Maybe Int)
  | WindowHint'AccumGreenBits         !(Maybe Int)
  | WindowHint'AccumBlueBits          !(Maybe Int)
  | WindowHint'AccumAlphaBits         !(Maybe Int)
  | WindowHint'AuxBuffers             !(Maybe Int)
  | WindowHint'Samples                !(Maybe Int)
  | WindowHint'RefreshRate            !(Maybe Int)
  | WindowHint'DoubleBuffer           !Bool
  | WindowHint'Stereo                 !Bool
  | WindowHint'sRGBCapable            !Bool
  | WindowHint'Floating               !Bool
  | WindowHint'Focused                !Bool
  | WindowHint'Maximized              !Bool
  | WindowHint'AutoIconify            !Bool
  | WindowHint'ClientAPI              !ClientAPI
  | WindowHint'ContextCreationAPI     !ContextCreationAPI
  | WindowHint'ContextVersionMajor    {-# UNPACK #-} !Int
  | WindowHint'ContextVersionMinor    {-# UNPACK #-} !Int
  | WindowHint'ContextRobustness      !ContextRobustness
  | WindowHint'ContextReleaseBehavior !ContextReleaseBehavior
  | WindowHint'ContextNoError         !Bool
  | WindowHint'OpenGLForwardCompat    !Bool
  | WindowHint'OpenGLDebugContext     !Bool
  | WindowHint'OpenGLProfile          !OpenGLProfile
  | WindowHint'TransparentFramebuffer !Bool
  | WindowHint'CenterCursor           !Bool
  | WindowHint'FocusOnShow            !Bool
  | WindowHint'ScaleToMonitor         !Bool
  | WindowHint'CocoaRetinaFramebuffer !Bool
  | WindowHint'CocoaGraphicsSwitching !Bool
  | WindowHint'CocoaFrameName         !String
  | WindowHint'X11ClassName           !String
  | WindowHint'X11InstanceName        !String
  deriving (Data, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData WindowHint

-- | A window-specific attribute.
-- See <https://www.glfw.org/docs/3.3/window_guide.html#window_attribs Window Attributes>
data WindowAttrib
  = WindowAttrib'Decorated
  | WindowAttrib'Resizable
  | WindowAttrib'Floating
  | WindowAttrib'AutoIconify
  | WindowAttrib'FocusOnShow
  | WindowAttrib'Hovered
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData WindowAttrib

-- | The OpenGL robustness strategy.
data ContextRobustness =
    ContextRobustness'NoRobustness
  | ContextRobustness'NoResetNotification
  | ContextRobustness'LoseContextOnReset
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData ContextRobustness

-- | The OpenGL profile.
data OpenGLProfile =
    OpenGLProfile'Any
  | OpenGLProfile'Compat
  | OpenGLProfile'Core
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData OpenGLProfile

-- | The type of OpenGL to create a context for.
data ClientAPI =
    ClientAPI'NoAPI
  | ClientAPI'OpenGL
  | ClientAPI'OpenGLES
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData ClientAPI

-- | The type of API to use for context creation.
-- See the <http://www.glfw.org/docs/latest/window_guide.html Window Guide> for
-- more information.
--
-- This is a hard constraint. If no client API is requested, this hint is
-- ignored. Best practice is to stick to one API or the other, otherwise may
-- segfault on Linux. OS X does not support the EGL API and will fail if this
-- hint is used.
data ContextCreationAPI
  = ContextCreationAPI'Native
  | ContextCreationAPI'EGL
  | ContextCreationAPI'OSMesa
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData ContextCreationAPI

-- | The context release behavior.
-- See the <http://www.glfw.org/docs/latest/window_guide.html Window Guide> for
-- more information.
--
-- Context release behaviors are described in detail by the
-- <https://www.khronos.org/registry/OpenGL/extensions/KHR/KHR_context_flush_control.txt KHR_context_flush_control>
-- extension.
data ContextReleaseBehavior
  = ContextReleaseBehavior'Any
  | ContextReleaseBehavior'None
  | ContextReleaseBehavior'Flush
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData ContextReleaseBehavior

--------------------------------------------------------------------------------
-- Input handling

-- | Part of the <http://www.glfw.org/docs/3.3/input.html#input_keyboard Keyboard Input> system.
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
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData Key

-- | The state of an individual key when 'Graphics.UI.GLFW.getKey' is called.
data KeyState =
    KeyState'Pressed
  | KeyState'Released
  | KeyState'Repeating
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData KeyState

-- | For use with the <http://www.glfw.org/docs/3.3/input.html#joystick Joystick Input> system.
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
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData Joystick

-- | If a given joystick button is pressed or not when
-- 'Graphics.UI.GLFW.getJoystickButtons' is called.
data JoystickButtonState =
    JoystickButtonState'Pressed
  | JoystickButtonState'Released
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData JoystickButtonState

-- | Part of the t'Graphics.UI.GLFW.JoystickCallback', for when a monitor gets
-- connected or disconnected.
data JoystickState
  = JoystickState'Connected
  | JoystickState'Disconnected
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData JoystickState

-- | The valid hat states of a joystick. Part of the
-- <https://www.glfw.org/docs/3.3/input_guide.html#joystick_hat joystick hat>
-- system.
data JoystickHatState
  = JoystickHatState'Centered
  | JoystickHatState'Up
  | JoystickHatState'Right
  | JoystickHatState'Down
  | JoystickHatState'Left
  | JoystickHatState'RightUp
  | JoystickHatState'RightDown
  | JoystickHatState'LeftUp
  | JoystickHatState'LeftDown
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData JoystickHatState

-- | Part of the <http://www.Graphics.UI.GLFW.org/docs/3.3/input.html#input_mouse Mouse Input> system.
data MouseButton =
    MouseButton'1
  | MouseButton'2
  | MouseButton'3
  | MouseButton'4
  | MouseButton'5
  | MouseButton'6
  | MouseButton'7
  | MouseButton'8
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData MouseButton

-- | If the mouse button is pressed or not when 'Graphics.UI.GLFW.getMouseButton' is
-- called.
data MouseButtonState =
    MouseButtonState'Pressed
  | MouseButtonState'Released
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData MouseButtonState

-- | If the mouse's cursor is in the window or not.
data CursorState =
    CursorState'InWindow
  | CursorState'NotInWindow
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData CursorState

-- | Allows for special forms of mouse input.
-- See <http://www.glfw.org/docs/3.3/input.html#cursor_mode Cursor Modes>
data CursorInputMode =
    CursorInputMode'Normal
  | CursorInputMode'Hidden
  | CursorInputMode'Disabled
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData CursorInputMode

-- | When sticky keys is enabled, once a key is pressed it will remain pressed
-- at least until the state is polled with 'Graphics.UI.GLFW.getKey'. After
-- that, if the key has been released it will switch back to released. This
-- helps prevent problems with low-resolution polling missing key pressed. Note
-- that use of the callbacks to avoid this problem the the recommended route,
-- and this is just for a fallback.
data StickyKeysInputMode =
    StickyKeysInputMode'Enabled
  | StickyKeysInputMode'Disabled
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData StickyKeysInputMode

-- | This is the mouse version of "StickyKeysInputMode".
data StickyMouseButtonsInputMode =
    StickyMouseButtonsInputMode'Enabled
  | StickyMouseButtonsInputMode'Disabled
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData StickyMouseButtonsInputMode

-- | Modifier keys that were pressed as part of another keypress event.
data ModifierKeys = ModifierKeys
  { modifierKeysShift    :: !Bool
  , modifierKeysControl  :: !Bool
  , modifierKeysAlt      :: !Bool
  , modifierKeysSuper    :: !Bool
  , modifierKeysCapsLock :: !Bool
  , modifierKeysNumLock  :: !Bool
  } deriving (Data, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData ModifierKeys

-- | The different types of buttons we can find on a Gamepad.
data GamepadButton
  = GamepadButton'A
  | GamepadButton'B
  | GamepadButton'X
  | GamepadButton'Y
  | GamepadButton'LeftBumper
  | GamepadButton'RightBumper
  | GamepadButton'Back
  | GamepadButton'Start
  | GamepadButton'Guide
  | GamepadButton'LeftThumb
  | GamepadButton'RightThumb
  | GamepadButton'DpadUp
  | GamepadButton'DpadRight
  | GamepadButton'DpadDown
  | GamepadButton'DpadLeft
  | GamepadButton'Cross
  | GamepadButton'Circle
  | GamepadButton'Square
  | GamepadButton'Triangle
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData GamepadButton

-- | The states in which the gamepad buttons are found
data GamepadButtonState
  = GamepadButtonState'Pressed
  | GamepadButtonState'Released
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData GamepadButtonState

-- | The different axes along which we can measure continuous input on a Gamepad
data GamepadAxis
  = GamepadAxis'LeftX
  | GamepadAxis'LeftY
  | GamepadAxis'RightX
  | GamepadAxis'RightY
  | GamepadAxis'LeftTrigger
  | GamepadAxis'RightTrigger
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData GamepadAxis

-- | This describes the input state of a gamepad
data GamepadState = GamepadState
                    { getButtonState :: GamepadButton -> GamepadButtonState
                      -- ^ Returns the current state of the given button
                    , getAxisState :: GamepadAxis -> Float
                      -- ^ Returns a value in the range [-1.0, 1.0] for the
                      -- given game axis
                    } deriving (Typeable, Generic)

instance Eq GamepadState where
  a == b =
    let compareSt f x = (&& (f a x == f b x))
     in foldr (compareSt getButtonState) True [minBound..maxBound] &&
        foldr (compareSt getAxisState) True [minBound..maxBound]

instance NFData GamepadState

deriving instance Data CUChar

-- | GLFW image data, for setting up custom mouse cursor appearnaces.
data Image = Image
  { imageWidth  :: {-# UNPACK #-} !Int
  , imageHeight :: {-# UNPACK #-} !Int
  , imagePixels :: [CUChar]
  } deriving (Data, Eq, Ord, Read, Show, Typeable, Generic)

-- | Create an image given the function to generate 8-bit RGBA values based on
-- the pixel location.
mkImage :: Int -> Int -> (Int -> Int -> (Word8, Word8, Word8, Word8)) -> Image
mkImage width height gen = Image
  { imageWidth = width
  , imageHeight = height
  , imagePixels = [ CUChar channel | y <- [0..(height - 1)]
                                   , x <- [0..(width - 1)]
                                   , let (r, g, b, a) = gen x y
                                   , channel <- [r, g, b, a]
                  ]
  }

instance NFData Image

-- | Represents a GLFW cursor.
newtype Cursor = Cursor
  { unCursor :: Ptr C'GLFWcursor
  } deriving (Data, Eq, Ord, Show, Typeable, Generic)

-- | Lets you use one of the standard cursor appearnaces that the local
-- system theme provides for.
-- See <http://www.glfw.org/docs/3.3/input.html#cursor_standard Standard Cursor Creation>.
data StandardCursorShape =
    StandardCursorShape'Arrow
  | StandardCursorShape'IBeam
  | StandardCursorShape'Crosshair
  | StandardCursorShape'Hand
  | StandardCursorShape'HResize
  | StandardCursorShape'VResize
  deriving (Bounded, Data, Enum, Eq, Ord, Read, Show, Typeable, Generic)

instance NFData StandardCursorShape

--------------------------------------------------------------------------------

{-# ANN module "HLint: ignore Use camelCase" #-}
