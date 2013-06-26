{-# LANGUAGE    TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- To derive instances of C c h with deriveC, c and h must be instances of
-- Lift.

module Graphics.UI.GLFW.Internal.Instances.Lift where

--------------------------------------------------------------------------------

import Foreign.C.Types            (CInt, CUChar)
import Language.Haskell.TH.Lift   (deriveLiftMany)
import Language.Haskell.TH.Syntax (Exp(LitE), Lift(..), Lit(IntegerL))

import Graphics.UI.GLFW.Types

--------------------------------------------------------------------------------

instance Lift CInt where
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift CUChar where
  lift x = return (LitE (IntegerL (fromIntegral x)))

deriveLiftMany
  [ ''ClientAPI
  , ''ContextRobustness
  , ''CursorInputMode
  , ''CursorState
  , ''Error
  , ''FocusState
  , ''IconifyState
  , ''Joystick
  , ''JoystickButtonState
  , ''Key
  , ''KeyState
  , ''MonitorState
  , ''MouseButton
  , ''MouseButtonState
  , ''OpenGLProfile
  , ''StickyKeysInputMode
  , ''StickyMouseButtonsInputMode
  ]
