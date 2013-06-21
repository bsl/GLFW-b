{-# LANGUAGE    TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphics.UI.GLFW.Internal.Instances.Data where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

import Data.Data
import Data.DeriveTH   (derives, makeData)
import Foreign.C.Types (CInt(..), CUChar(..))

import Graphics.UI.GLFW.Types

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

derives [makeData]
  [ ''CInt
  , ''CUChar
  , ''CursorInputMode
  , ''KeysInputMode
  , ''MouseButtonsInputMode
  , ''ClientApi
  , ''ContextRobustness
  , ''OpenglProfile
  , ''Error
  , ''CursorAction
  , ''FocusAction
  , ''IconifyAction
  , ''KeyAction
  , ''JoystickButtonAction
  , ''MouseButtonAction
  , ''MonitorAction
  , ''Key
  , ''MouseButton
  , ''WindowAttribute
  , ''Joystick
  ]
