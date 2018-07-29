
WAT2WASM=../wasm/wabt/bin/wat2wasm

%.wasm : %.wat
	$(WAT2WASM) $<

all : fib.wasm

