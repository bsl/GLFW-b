{-# LANGUAGE    FlexibleInstances     #-}
{-# LANGUAGE    MultiParamTypeClasses #-}
{-# LANGUAGE    TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-orphans     #-}

module Graphics.UI.GLFW.Internal.Instances.C where

--------------------------------------------------------------------------------

import Data.Bits       ((.&.))
import Data.Char       (chr, ord)
import Foreign.C.Types (CDouble, CFloat, CInt, CUChar, CUInt, CUShort)
import Foreign.Ptr     (Ptr)

import Bindings.GLFW
import Graphics.UI.GLFW.Internal.C              (C(..))
import Graphics.UI.GLFW.Internal.C.TH           (deriveC)
import Graphics.UI.GLFW.Internal.Instances.Data ()
import Graphics.UI.GLFW.Internal.Instances.Lift ()
import Graphics.UI.GLFW.Internal.Tables
import Graphics.UI.GLFW.Types

--------------------------------------------------------------------------------

deriveC ''CInt   ''Bool                        table_C_CInt_Bool
deriveC ''CInt   ''ClientAPI                   table_C_CInt_ClientAPI
deriveC ''CInt   ''ContextRobustness           table_C_CInt_ContextRobustness
deriveC ''CInt   ''CursorInputMode             table_C_CInt_CursorInputMode
deriveC ''CInt   ''CursorState                 table_C_CInt_CursorState
deriveC ''CInt   ''Error                       table_C_CInt_Error
deriveC ''CInt   ''FocusState                  table_C_CInt_FocusState
deriveC ''CInt   ''IconifyState                table_C_CInt_IconifyState
deriveC ''CInt   ''Joystick                    table_C_CInt_Joystick
deriveC ''CInt   ''Key                         table_C_CInt_Key
deriveC ''CInt   ''KeyState                    table_C_CInt_KeyState
deriveC ''CInt   ''MonitorState                table_C_CInt_MonitorState
deriveC ''CInt   ''MouseButton                 table_C_CInt_MouseButton
deriveC ''CInt   ''MouseButtonState            table_C_CInt_MouseButtonState
deriveC ''CInt   ''OpenGLProfile               table_C_CInt_OpenGLProfile
deriveC ''CInt   ''StickyKeysInputMode         table_C_CInt_StickyKeysInputMode
deriveC ''CInt   ''StickyMouseButtonsInputMode table_C_CInt_StickyMouseButtonsInputMode
deriveC ''CUChar ''JoystickButtonState         table_C_CUChar_JoystickButtonState

instance C CInt Char where
  fromC = chr . fromIntegral
  toC   = fromIntegral . ord

instance C CUInt Char where
  fromC = chr . fromIntegral
  toC   = fromIntegral . ord

instance C CDouble Double where
  fromC = realToFrac
  toC   = realToFrac

instance C CInt Int where
  fromC = fromIntegral
  toC   = fromIntegral

instance C CUInt Int where
  fromC = fromIntegral
  toC   = fromIntegral

instance C CUShort Int where
  fromC = fromIntegral
  toC   = fromIntegral

instance C CFloat Double where
  fromC = realToFrac
  toC   = realToFrac

instance C (Ptr C'GLFWmonitor) Monitor where
  fromC = Monitor
  toC   = unMonitor

instance C (Ptr C'GLFWwindow) Window where
  fromC = Window
  toC   = unWindow

instance C CInt ModifierKeys where
  fromC v = ModifierKeys
    { modifierKeysShift   = (v .&. c'GLFW_MOD_SHIFT)   /= 0
    , modifierKeysControl = (v .&. c'GLFW_MOD_CONTROL) /= 0
    , modifierKeysAlt     = (v .&. c'GLFW_MOD_ALT)     /= 0
    , modifierKeysSuper   = (v .&. c'GLFW_MOD_SUPER)   /= 0
    }
  toC = undefined

instance C C'GLFWvidmode VideoMode where
  fromC gvm = VideoMode
    { videoModeWidth       = fromIntegral $ c'GLFWvidmode'width       gvm
    , videoModeHeight      = fromIntegral $ c'GLFWvidmode'height      gvm
    , videoModeRedBits     = fromIntegral $ c'GLFWvidmode'redBits     gvm
    , videoModeGreenBits   = fromIntegral $ c'GLFWvidmode'greenBits   gvm
    , videoModeBlueBits    = fromIntegral $ c'GLFWvidmode'blueBits    gvm
    , videoModeRefreshRate = fromIntegral $ c'GLFWvidmode'refreshRate gvm
    }
  toC = undefined
