{-# LANGUAGE    MultiParamTypeClasses #-}
{-# LANGUAGE    TemplateHaskell       #-}
{-# OPTIONS_GHC -fno-warn-orphans     #-}

#include <GLFW/glfw3.h>

module Graphics.UI.GLFW.Internal.Instances.C where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Data.Bits       ((.&.))
import Data.Char       (chr, ord)
import Foreign.C.Types (CDouble, CFloat, CInt, CUChar, CUInt, CUShort)

import Graphics.UI.GLFW.Internal.C              (C(..))
import Graphics.UI.GLFW.Internal.C.TH           (deriveC)
import Graphics.UI.GLFW.Internal.Instances.Data ()
import Graphics.UI.GLFW.Internal.Instances.Lift ()
import Graphics.UI.GLFW.Internal.Tables
import Graphics.UI.GLFW.Types

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

deriveC ''CInt   ''Bool                  table_C_CInt_Bool
deriveC ''CInt   ''CursorInputMode       table_C_CInt_CursorInputMode
deriveC ''CInt   ''KeysInputMode         table_C_CInt_KeysInputMode
deriveC ''CInt   ''MouseButtonsInputMode table_C_CInt_MouseButtonsInputMode
deriveC ''CInt   ''ClientApi             table_C_CInt_ClientApi
deriveC ''CInt   ''ContextRobustness     table_C_CInt_ContextRobustness
deriveC ''CInt   ''OpenglProfile         table_C_CInt_OpenglProfile
deriveC ''CInt   ''Error                 table_C_CInt_Error
deriveC ''CInt   ''CursorAction          table_C_CInt_CursorAction
deriveC ''CInt   ''FocusAction           table_C_CInt_FocusAction
deriveC ''CInt   ''IconifyAction         table_C_CInt_IconifyAction
deriveC ''CInt   ''KeyAction             table_C_CInt_KeyAction
deriveC ''CUChar ''JoystickButtonAction  table_C_CUChar_JoystickButtonAction
deriveC ''CInt   ''MouseButtonAction     table_C_CInt_MouseButtonAction
deriveC ''CInt   ''MonitorAction         table_C_CInt_MonitorAction
deriveC ''CInt   ''Key                   table_C_CInt_Key
deriveC ''CInt   ''MouseButton           table_C_CInt_MouseButton
deriveC ''CInt   ''WindowAttribute       table_C_CInt_WindowAttribute
deriveC ''CInt   ''Joystick              table_C_CInt_Joystick

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

instance C CInt ModifierKeys where
  fromC v = ModifierKeys
    { modifierKeysShift   = (v .&. (#const GLFW_MOD_SHIFT))   /= 0
    , modifierKeysControl = (v .&. (#const GLFW_MOD_CONTROL)) /= 0
    , modifierKeysAlt     = (v .&. (#const GLFW_MOD_ALT))     /= 0
    , modifierKeysSuper   = (v .&. (#const GLFW_MOD_SUPER))   /= 0
    }

instance C GlfwVideoMode VideoMode where
  fromC gvm = VideoMode
    { videoModeWidth       = fromIntegral $ glfwVideoModeWidth       gvm
    , videoModeHeight      = fromIntegral $ glfwVideoModeHeight      gvm
    , videoModeRedBits     = fromIntegral $ glfwVideoModeRedBits     gvm
    , videoModeGreenBits   = fromIntegral $ glfwVideoModeGreenBits   gvm
    , videoModeBlueBits    = fromIntegral $ glfwVideoModeBlueBits    gvm
    , videoModeRefreshRate = fromIntegral $ glfwVideoModeRefreshRate gvm
    }
