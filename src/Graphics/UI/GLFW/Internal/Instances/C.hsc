{-# LANGUAGE    FlexibleInstances     #-}
{-# LANGUAGE    MultiParamTypeClasses #-}
{-# LANGUAGE    TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-orphans     #-}

#include <GLFW/glfw3.h>

module Graphics.UI.GLFW.Internal.Instances.C where

--------------------------------------------------------------------------------

import Data.Bits       ((.&.))
import Data.Char       (chr, ord)
import Foreign.C.Types (CDouble, CFloat, CInt, CUChar, CUInt, CUShort)
import Foreign.Ptr     (Ptr)

import Graphics.UI.GLFW.Internal.C              (C(..))
import Graphics.UI.GLFW.Internal.C.TH           (deriveC)
import Graphics.UI.GLFW.Internal.Instances.Data ()
import Graphics.UI.GLFW.Internal.Instances.Lift ()
import Graphics.UI.GLFW.Internal.Tables
import Graphics.UI.GLFW.Types

--------------------------------------------------------------------------------

deriveC ''CInt   ''Bool                        table_C_CInt_Bool
deriveC ''CInt   ''CursorInputMode             table_C_CInt_CursorInputMode
deriveC ''CInt   ''StickyKeysInputMode         table_C_CInt_StickyKeysInputMode
deriveC ''CInt   ''StickyMouseButtonsInputMode table_C_CInt_StickyMouseButtonsInputMode
deriveC ''CInt   ''ClientAPI                   table_C_CInt_ClientAPI
deriveC ''CInt   ''ContextRobustness           table_C_CInt_ContextRobustness
deriveC ''CInt   ''OpenGLProfile               table_C_CInt_OpenGLProfile
deriveC ''CInt   ''Error                       table_C_CInt_Error
deriveC ''CInt   ''CursorState                 table_C_CInt_CursorState
deriveC ''CInt   ''FocusState                  table_C_CInt_FocusState
deriveC ''CInt   ''IconifyState                table_C_CInt_IconifyState
deriveC ''CInt   ''KeyState                    table_C_CInt_KeyState
deriveC ''CUChar ''JoystickButtonState         table_C_CUChar_JoystickButtonState
deriveC ''CInt   ''MouseButtonState            table_C_CInt_MouseButtonState
deriveC ''CInt   ''MonitorState                table_C_CInt_MonitorState
deriveC ''CInt   ''Key                         table_C_CInt_Key
deriveC ''CInt   ''MouseButton                 table_C_CInt_MouseButton
deriveC ''CInt   ''Joystick                    table_C_CInt_Joystick

instance C CInt Char where
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

instance C (Ptr GlfwMonitor) Monitor where
  fromC = Monitor
  toC   = unMonitor

instance C (Ptr GlfwWindow) Window where
  fromC = Window
  toC   = unWindow

instance C CInt ModifierKeys where
  fromC v = ModifierKeys
    { modifierKeysShift   = (v .&. (#const GLFW_MOD_SHIFT))   /= 0
    , modifierKeysControl = (v .&. (#const GLFW_MOD_CONTROL)) /= 0
    , modifierKeysAlt     = (v .&. (#const GLFW_MOD_ALT))     /= 0
    , modifierKeysSuper   = (v .&. (#const GLFW_MOD_SUPER))   /= 0
    }
  toC = undefined

instance C GlfwVideoMode VideoMode where
  fromC gvm = VideoMode
    { videoModeWidth       = fromIntegral $ glfwVideoModeWidth       gvm
    , videoModeHeight      = fromIntegral $ glfwVideoModeHeight      gvm
    , videoModeRedBits     = fromIntegral $ glfwVideoModeRedBits     gvm
    , videoModeGreenBits   = fromIntegral $ glfwVideoModeGreenBits   gvm
    , videoModeBlueBits    = fromIntegral $ glfwVideoModeBlueBits    gvm
    , videoModeRefreshRate = fromIntegral $ glfwVideoModeRefreshRate gvm
    }
  toC = undefined
