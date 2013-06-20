{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE TemplateHaskell #-}

module Graphics.UI.GLFW.C where

-- import Language.Haskell.TH

class C c h where
  fromC :: c -> h
  toC   :: h -> c
  fromC = undefined
  toC   = undefined

-- deriveC :: Name -> Name -> [(c, h)] -> DecQ
-- deriveC cname hname pairs =
--     instanceD
--       (cxt [])
--       (appT (appT (conT ''C) (conT cname)) (conT hname))
--       [funD (mkName "fromC") $ map genClause pairs]
--   where
--     genClause (cv, hv) = clause [] (normalB [|hv|]) []




-- $(reify ''Int >>= stringE . show)
-- data A = A { a0 :: Int, a1 :: Int }
-- class C c h where fromC :: c -> h
-- runQ [d|instance C A Int where; fromC (A 0 0) = 0|]
