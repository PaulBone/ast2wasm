
= lambda2wasm

I'm experimenting with WebAssembly, an intermediate code representation for
the web (and elsewhere).  So I'm writing a code generator that targets it.

Copyright 2018 Paul Bone
License: MIT

== Building

I tested this on Linux.

You'll need the wabt git submodule, it contains the WebAssembly Binary
Toolkit, mainly the wat2wasm tool,  after you checkout do:

    $ git submodule init .
    $ git submodule update --checkout .

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


