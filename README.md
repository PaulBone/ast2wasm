
= lambda2wasm

I'm experimenting with WebAssembly, an intermediate code representation for
the web (and elsewhere).  So I'm writing a code generator that targets it.

Copyright 2018 Paul Bone
License: MIT


== Dependencies

You'll need some kind of C++ toolchain, whatever wabt (see below) requires.
Plus Haskell and Cabal.  And a modern browser that support WebAssembly
(pretty much anything current & mainstream except IE) I recommend Firefox
;-).


== Building

I tested this on Linux.

You'll need the wabt git submodule, it contains the WebAssembly Binary
Toolkit, mainly the wat2wasm tool,  after you checkout do:

    $ git submodule init .
    $ git submodule update --checkout .

Part of the build includes downloading Haskell packages (into the local
directory via cabal).  You'll need to update cabal package lists first:

    $ cabal update

You should be able to build the toolkit and this with:

    $ make

It may need more tools or libraries installed such as cmake, C header files
and libraries.


== Links

I used these resources:

 * [Web Assembly website](http://webassembly.org)
 * [Wasm text format and instruction list](https://webassembly.github.io/spec/core/text/instructions.html)
 * [Wasm text format examples](https://github.com/WebAssembly/spec/tree/master/test/core)
 * [MDN docs](https://developer.mozilla.org/en-US/docs/WebAssembly) Mostly
  JavaScript API for loading modules.
 * [Wasm Assembler](https://github.com/webassembly/wabt)

While researching this I also found, but didn't use:

 * [wasm Haskell package](https://hackage.haskell.org/package/wasm)


== TODO

 * add ctof/ftoc example
 * Benchmark with JS and native code, use a suitable benchmark


== FAQ

Questions that people have asked me I will need to include in the
presentation.

 * script tag
 * Loading and linking multiple modules


