
WAT2WASM=wabt/build/wat2wasm

.PHONY: all
all : ctof.wasm \
	fib_handwritten.wasm \
	calls.wasm \
	quad.wasm \
	let.wasm \
	.cabal-sandbox/bin/ast2wasm

%.wasm : %.wat $(WAT2WASM)
	$(WAT2WASM) $<
	touch $@

%.wat : %.hl .cabal-sandbox/bin/ast2wasm
	.cabal-sandbox/bin/ast2wasm < $< > $@

$(WAT2WASM) :
	-mkdir wabt/build
	( cd wabt/build/; cmake -DBUILD_TESTS=OFF .. ; make wat2wasm )

.cabal-sandbox/bin/ast2wasm: .cabal-sandbox $(wildcard *.hs)
	cabal install -j

.cabal-sandbox: ast2wasm.cabal
	cabal sandbox init
	touch .sandbox_guard

