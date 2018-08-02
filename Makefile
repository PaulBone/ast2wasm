
WAT2WASM=wabt/build/wat2wasm

%.wasm : %.wat wat2wasm
	$(WAT2WASM) $<

.PHONY: all
all : fib.wasm .cabal-sandbox/bin/ast2wasm

.PHONY: wat2wasm
wat2wasm : .wat2wasm_guard

.wat2wasm_guard :
	-mkdir wabt/build
	( cd wabt/build/; cmake -DBUILD_TESTS=OFF .. ; make wat2wasm )
	touch .wat2wasm_guard

.cabal-sandbox/bin/ast2wasm: sandbox $(find *.hs)
	cabal install -j

.PHONY: sandbox
sandbox: .sandbox_guard

.sandbox_guard: ast2wasm.cabal
	cabal sandbox init
	touch .sandbox_guard

