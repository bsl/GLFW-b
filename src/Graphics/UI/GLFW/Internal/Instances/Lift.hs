{-# LANGUAGE    TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
  [ ''CursorInputMode
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
