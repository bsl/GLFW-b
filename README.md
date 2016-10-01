GLFW-b
======
[![Hackage](https://img.shields.io/hackage/v/GLFW-b.svg)](http://hackage.haskell.org/package/GLFW-b)

## Description

[Haskell][1] bindings to [GLFW][2], an Open Source, multi-platform library for
creating windows with OpenGL contexts and managing input and events.

GLFW-b depends on [bindings-GLFW][3], which, as of the time of this writing,
binds to GLFW 3.1, [released 2015-01-18][4].

For a demonstration of GLFW-b, see [GLFW-b-demo][5].

When running GLFW-b in GHCI, don't forget to

    :set -fno-ghci-sandbox

since GHCI can (and might) run each line of your code in a fresh unbounded
thread.

## Contributing

This package uses git-flow as development model, in short that means that:

1. New features should be added to "develop" branch.
2. "master" branch is reserved for stable releases.
3. Patches for bugs related with previous releases should always be done in
    "hotfixes" branch.
4. All merge commits to master from "hotfixes" should be done
    using the "--no-ff" flag and from "develop" should avoid merging commits.

Until we have a defined road-map we are going to leave out "release
"branches" and "feature branches". For more information about this development
model please refer to [this site.][6]

Thanks you, and happy coding.

[1]: http://www.haskell.org/
[2]: http://www.glfw.org/
[3]: https://github.com/bsl/bindings-GLFW
[4]: http://www.glfw.org/Version-3.1-released.html
[5]: https://github.com/bsl/GLFW-b-demo
[6]: http://nvie.com/posts/a-successful-git-branching-model/
