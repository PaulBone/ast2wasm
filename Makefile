
WAT2WASM=wabt/build/wat2wasm

%.wasm : %.wat
	$(WAT2WASM) $<

all : wat2wasm fib.wasm

.PHONY: wat2wasm
wat2wasm : .wat2wasm_guard

.wat2wasm_guard :
	-mkdir wabt/build
	( cd wabt/build/; cmake -DBUILD_TESTS=OFF .. ; make wat2wasm )
	touch .wat2wasm_guard

