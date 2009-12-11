Differences between GLFW and GLFW-b 2009/12
===========================================

* GLFW binds to version 2.6 of the GLFW C library. GLFW-b binds to 2.7.

* Some GLFW functions return OpenGL {Gettable,Settable}StateVars. I don't like
  this because it adds an extra step to extract a value and causes GLFW to be
  dependent on OpenGL. GLFW-b doesn't depend on OpenGL.

* GLFW sometimes depends on GLFW C library internally defined values. GLFW-b
  doesn't. For example, GLFW-b doesn't assume that GL\_FALSE = 0. I don't think
  this makes a difference in practice, but it makes me feel better.

* GLFW sometimes assumes that certain Haskell types are equivalent to certain C
  types, e.g. Int and int. GLFW-b doesn't. GLFW-b always uses the appropriate
  type from Foreign.C.Types. I don't think this makes a difference in practice,
  but it makes me feel better.

* GLFW contains workarounds for a bugs in GHC &lt; 6.10 FFI. GLFW-b doesn't.

* GLFW's API is pretty faithful to the C API. GLFW-b prefers to use full words
  and self-explanatory function names.

* In GLFW-b, registering hints and opening a window are combined into one step.

* GLFW has support for loading textures and rendering strings. GLFW-b doesn't.
