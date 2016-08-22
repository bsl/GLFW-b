This package is looking for a new maintainer
============================================

[![Join the chat at https://gitter.im/haskell-glfw/Lobby](https://badges.gitter.im/haskell-glfw/Lobby.svg)](https://gitter.im/haskell-glfw/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)
`bindings-GLFW` and `GLFW-b` are looking for a new maintainer. Please comment in [this issue](https://github.com/bsl/bindings-GLFW/issues/39) if you would like to take maintainership.

GLFW-b
======
[![Hackage](https://img.shields.io/hackage/v/GLFW-b.svg)](http://hackage.haskell.org/package/GLFW-b)

[Haskell][1] bindings to [GLFW][2], an Open Source, multi-platform library for
creating windows with OpenGL contexts and managing input and events.

GLFW-b depends on [bindings-GLFW][3], which, as of the time of this writing,
binds to GLFW 3.1, [released 2015-01-18][4].

For a demonstration of GLFW-b, see [GLFW-b-demo][5].

When running GLFW-b in GHCI, don't forget to 

    :set -fno-ghci-sandbox

since GHCI can (and might) run each line of your code in a fresh unbounded 
thread.

[1]: http://www.haskell.org/
[2]: http://www.glfw.org/
[3]: https://github.com/bsl/bindings-GLFW
[4]: http://www.glfw.org/Version-3.1-released.html
[5]: https://github.com/bsl/GLFW-b-demo
