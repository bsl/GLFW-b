{-# LANGUAGE MultiParamTypeClasses #-}

module Graphics.UI.GLFW.Internal.C where

-- A class for converting between C-type c and Haskell-type h. For example,
-- suppose you have a Haskell Joystick, but need the CInt, or vice versa.
class C c h where
  fromC :: c -> h
  toC   :: h -> c
