{-# LANGUAGE TemplateHaskell #-}

-- deriveC creates an instance of C c h from [(c, h)].
--
-- Example:
--
-- > pairs :: [(CInt, Bool)]
-- > pairs = [(0, False), (1, True)]
-- >
-- > deriveC ''CInt ''Bool pairs
--
-- would derive
--
-- > instance C CInt Bool where
-- >   fromC 0 = False
-- >   fromC 1 = True
-- >   fromC _ = error ...
-- >
-- >   toC False = 0
-- >   toC True  = 1

module Graphics.UI.GLFW.Internal.C.TH
  ( deriveC
  ) where

--------------------------------------------------------------------------------

import Data.Data                  (Data)
import Data.Tuple                 (swap)
import Language.Haskell.TH        (Dec, Name, Q, appT, clause, conT, cxt, funD, global, instanceD, mkName, nameBase, normalB, varP)
import Language.Haskell.TH.Quote  (dataToPatQ)
import Language.Haskell.TH.Syntax (Lift(..))

--------------------------------------------------------------------------------

deriveC :: (Data c, Data h, Lift c, Lift h) => Name -> Name -> [(c, h)] -> Q [Dec]
deriveC ctype htype pairs =
    (:[]) `fmap` deriveC1 ctype htype pairs

--------------------------------------------------------------------------------

deriveC1 :: (Data c, Data h, Lift c, Lift h) => Name -> Name -> [(c, h)] -> Q Dec
deriveC1 _ _ [] =
    error "deriveC: no pairs!"
deriveC1 ctype htype pairs =
    instanceD
      (cxt [])
      (appT
          (appT (conT cname) (conT ctype))
          (conT htype)
      )
      [ funD frname frClauses
      , funD toname toClauses
      ]
  where
    cname  = mkName "C"
    frname = mkName "fromC"
    toname = mkName "toC"
    vname  = mkName "v"

    frClauses = map  genPairClause         pairs ++ [genErrorClause]
    toClauses = map (genPairClause . swap) pairs

    genPairClause (v0, v1) =
        clause [pat] body []
      where
        pat  = dataToPatQ (const Nothing) v0
        body = normalB [|v1|]

    genErrorClause =
        clause [pat] body []
      where
        pat   = varP vname
        body  = normalB [| error $ msg ++ show $(global vname) |]
        -- e.g., "C CInt Bool fromC: "
        msg = unwords (map nameBase [cname, ctype, htype, frname]) ++ ": "
