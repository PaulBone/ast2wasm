
function load_wasm(output) {
    /*
     * There's about three ways to load a wasm module right now.  Some are
     * "better" and others work in more browsers:
     *
     * + Streaming compilation
     * + Non-streaming async
     * + Non-streaming sync
     *
     * In the future browsers will also be able to use a <script> tag, but
     * for now that's not available.
     *
     * Since I'm not concerned about browser compatibility I would have used
     * streaming compilation as it is also the simplist, but it seems like
     * it doesn't work with the fetch API when the .wasm file is a local
     * file (due to mime types).  So I'll use the non-streaming async
     * method.
     */
    output.innerText="Downloading...";
    const state = {
        error: false
    };

    const url = "file:///mnt/btrfs-hdd/dev/lambda2wasm/fib.wasm";
    /*
    // fetch belongs to the FetchAPI, it is not wasm-specific.
    const request = fetch(url).catch(r => {
        state.error = true;
        output.innerText =
            `Failed to download bytecode: ${r}`;
    });
    */
    const request = new XMLHttpRequest();
    request.open("GET", url);
    request.onload = response => {
        const importObj = { imports: {} };
        WebAssembly.instantiate(request.response, importObj).then(
            result => {
                output.innerText = "Ready";

            },
            error => {
                if (!state.error) {
                    state.error = true;
                    output.innerText =
                        `Failed to compile or instantiate bytecode: ${error}`;
                }
            });
        };
    request.onerror = error => {
            state.error = true;
            output.innerText =
                `Failed to download bytecode: ${error}`;
        };
    request.responseType = "arraybuffer";
    request.send();
}
