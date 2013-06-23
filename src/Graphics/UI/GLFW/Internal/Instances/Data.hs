{-# LANGUAGE    DeriveDataTypeable #-}
{-# LANGUAGE    StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}

module Graphics.UI.GLFW.Internal.Instances.Data where

-- --------------------------------------------------------------------------------

import Data.Data       (Data)
import Foreign.C.Types (CInt(..), CUChar(..))

-- --------------------------------------------------------------------------------

deriving instance Data CInt
deriving instance Data CUChar
