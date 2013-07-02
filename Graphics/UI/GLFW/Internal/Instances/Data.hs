{-# LANGUAGE    DeriveDataTypeable #-}
{-# LANGUAGE    StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

-- To derive instances of C c h with deriveC, c and h must be instances of
-- Data.

module Graphics.UI.GLFW.Internal.Instances.Data where

--------------------------------------------------------------------------------

import Data.Data       (Data)
import Foreign.C.Types (CInt(..), CUChar(..))

--------------------------------------------------------------------------------

deriving instance Data CInt
deriving instance Data CUChar
