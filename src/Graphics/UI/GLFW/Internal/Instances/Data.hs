{-# LANGUAGE    TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Graphics.UI.GLFW.Internal.Instances.Data where

--------------------------------------------------------------------------------

import Data.Data
import Data.DeriveTH   (derives, makeData)
import Foreign.C.Types (CInt(..), CUChar(..))

import Graphics.UI.GLFW.Types

--------------------------------------------------------------------------------

derives [makeData]
  [ ''CInt
  , ''CUChar
  , ''CursorInputMode
  , ''StickyKeysInputMode
  , ''StickyMouseButtonsInputMode
  , ''ClientAPI
  , ''ContextRobustness
  , ''OpenGLProfile
  , ''Error
  , ''CursorState
  , ''FocusState
  , ''IconifyState
  , ''KeyState
  , ''JoystickButtonState
  , ''MouseButtonState
  , ''MonitorState
  , ''Key
  , ''MouseButton
  , ''Joystick
  ]
