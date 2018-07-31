
function DownloadError(request) {
    this.status = request.status;
    this.statusText = request.statusText;
    this.toString = () => `${this.status}: ${this.statusText}`;
}

function download_wasm(url) {
    /*
     * We can't use the Fetch API when the .wasm file is a local
     * file (due to mime types).  So instead use XMLHttpRequest.
     */
    return new Promise((resolve, reject) => {
        const request = new XMLHttpRequest();
        request.open("GET", url);
        request.onload = _ => resolve(request.response);
        request.onerror = _ => reject(new DownloadError(request));
        request.responseType = "arraybuffer";
        request.send();
    });
}

async function load_wasm(reportStatus) {
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
    try {
        reportStatus("Downloading...");
        const url = "file:///mnt/btrfs-hdd/dev/lambda2wasm/fib.wasm";
        const response = await download_wasm(url);
        reportStatus("Compiling");
        const importObj = { imports: {} };
        result = await WebAssembly.instantiate(response, importObj);
        reportStatus("Ready");
        return result.instance.exports.fib;
    } catch(error) {
        let message;
        if (error instanceof DownloadError) {
            message = `Failed to download bytecode`;
        } else if (error instanceof WebAssembly.CompileError) {
            message = `Failed to compile module: ${error}`;
        } else if (error instanceof WebAssembly.LinkError) {
            message = `Failed to link module: ${error}`;
        } else if (error instanceof WebAssembly.RuntimeError) {
            message = `Other WebAssembly error: ${error}`;
        } else {
            message = `Other error: ${error}`;
        }
        reportStatus(message);
        throw new Error(message);
    };
}
