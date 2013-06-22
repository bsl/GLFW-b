{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.UI.GLFW.Internal.C
  ( C(..)
  ) where

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

class C c h where
  fromC :: c -> h
  toC   :: h -> c
  fromC = undefined
  toC   = undefined
