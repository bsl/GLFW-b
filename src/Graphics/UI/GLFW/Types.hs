{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls     #-}

module Graphics.UI.GLFW.Types where

--------------------------------------------------------------------------------

import Data.Typeable   (Typeable)
import Foreign.C.Types (CInt)
import Foreign.Ptr     (Ptr)

--------------------------------------------------------------------------------

data GlfwWindow
data GlfwGammaRamp
data GlfwMonitor

data GlfwVideoMode = GlfwVideoMode
  { glfwVideoModeWidth       :: CInt
  , glfwVideoModeHeight      :: CInt
  , glfwVideoModeRedBits     :: CInt
  , glfwVideoModeGreenBits   :: CInt
  , glfwVideoModeBlueBits    :: CInt
  , glfwVideoModeRefreshRate :: CInt
  } deriving (Eq, Show)

--------------------------------------------------------------------------------

data CursorInputMode =
    CursorNormal
  | CursorHidden
  | CursorDisabled
  deriving (Eq, Show, Typeable)

data GammaRamp = GammaRamp
  { gammaRampRed   :: [Int]
  , gammaRampGreen :: [Int]
  , gammaRampBlue  :: [Int]
  , gammaRampSize  :: Int
  } deriving (Eq, Show, Typeable)

data KeysInputMode =
    KeysNormal
  | KeysSticky
  deriving (Eq, Show, Typeable)

data MouseButtonsInputMode =
    MouseButtonsNormal
  | MouseButtonsSticky
  deriving (Eq, Show, Typeable)

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
  } deriving (Eq, Show, Typeable)

data ClientApi =
    OpenglApi
  | OpenglEsApi
  deriving (Eq, Show, Typeable)

data ContextRobustness =
    NoRobustness
  | NoResetNotification
  | LoseContextOnReset
  deriving (Eq, Show, Typeable)

data OpenglProfile =
    OpenglAnyProfile
  | OpenglCompatProfile
  | OpenglCoreProfile
  deriving (Eq, Show, Typeable)

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
  deriving (Eq, Show, Typeable)

data CursorAction =
    CursorEnter
  | CursorLeave
  deriving (Eq, Show, Typeable)

data FocusAction =
    Focus
  | Defocus
  deriving (Eq, Show, Typeable)

data IconifyAction =
    Iconify
  | Restore
  deriving (Eq, Show, Typeable)

data KeyAction =
    KeyPress
  | KeyRelease
  | KeyRepeat
  deriving (Eq, Show, Typeable)

data JoystickButtonAction =
    JoystickButtonPress
  | JoystickButtonRelease
  deriving (Eq, Show, Typeable)

data MouseButtonAction =
    MouseButtonPress
  | MouseButtonRelease
  deriving (Eq, Show, Typeable)

data MonitorAction =
    Connect
  | Disconnect
  deriving (Eq, Show, Typeable)

data ModifierKeys = ModifierKeys
  { modifierKeysShift   :: Bool
  , modifierKeysControl :: Bool
  , modifierKeysAlt     :: Bool
  , modifierKeysSuper   :: Bool
  } deriving (Eq, Show, Typeable)

data Version = Version
  { versionMajor    :: Int
  , versionMinor    :: Int
  , versionRevision :: Int
  } deriving (Eq, Ord, Show, Typeable)

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
  deriving (Eq, Show, Typeable)

data MouseButton =
    MouseButton1
  | MouseButton2
  | MouseButton3
  | MouseButton4
  | MouseButton5
  | MouseButton6
  | MouseButton7
  | MouseButton8
  deriving (Eq, Show, Typeable)

newtype Window = Window
  { unWindow :: Ptr GlfwWindow
  } deriving (Eq, Show, Typeable)

newtype Monitor = Monitor
  { unMonitor :: Ptr GlfwMonitor
  } deriving (Eq, Show, Typeable)

data VideoMode = VideoMode
  { videoModeWidth       :: Int
  , videoModeHeight      :: Int
  , videoModeRedBits     :: Int
  , videoModeGreenBits   :: Int
  , videoModeBlueBits    :: Int
  , videoModeRefreshRate :: Int
  } deriving (Eq, Show, Typeable)

data WindowAttribute =
    Focused
  | Iconified
  | Visible
  | Resizable
  deriving (Eq, Show, Typeable)

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
  deriving (Eq, Ord, Show, Typeable)
