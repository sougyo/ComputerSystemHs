'use strict';
// ============================================================
// Haskell/WASM ブリッジ
// GHC WASM バックエンドでビルドされた computersystem.wasm を
// ロードし、エクスポート関数をラップする
// ============================================================

const HaskellCPU = (() => {
    let wasmInstance = null;
    let memory = null;

    // WASM 初期化
    async function init(wasmPath) {
        const response = await fetch(wasmPath);
        const buffer   = await response.arrayBuffer();
        const result   = await WebAssembly.instantiate(buffer, {
            wasi_snapshot_preview1: makeWASI(),
        });
        wasmInstance = result.instance;
        memory       = wasmInstance.exports.memory;

        // Haskell ランタイム初期化 (GHC WASM が必要とする)
        if (wasmInstance.exports.hs_init) {
            wasmInstance.exports.hs_init(0, 0);
        }
        return true;
    }

    // C文字列をJSStringに変換 (WASM メモリから読む)
    function readCString(ptr) {
        const buf = new Uint8Array(memory.buffer);
        let end = ptr;
        while (buf[end] !== 0) end++;
        return new TextDecoder().decode(buf.slice(ptr, end));
    }

    // JSStringをWASMメモリに書き込みポインタを返す
    function writeCString(str) {
        const bytes = new TextEncoder().encode(str + '\0');
        const ptr   = wasmInstance.exports.malloc(bytes.length);
        const buf   = new Uint8Array(memory.buffer);
        buf.set(bytes, ptr);
        return ptr;
    }

    // ── 公開API ────────────────────────────────

    // CPUレイアウトJSON (初期化時に1度)
    function getLayout() {
        const ptr = wasmInstance.exports.hs_get_layout();
        return JSON.parse(readCString(ptr));
    }

    // CPUステートJSON
    function getState() {
        const ptr = wasmInstance.exports.hs_get_state();
        return JSON.parse(readCString(ptr));
    }

    // ワイヤ値配列 [{id, v}, ...]
    function getWires() {
        const ptr = wasmInstance.exports.hs_get_wires();
        return JSON.parse(readCString(ptr));
    }

    // 1ステップ実行
    function step() {
        wasmInstance.exports.hs_step();
    }

    // リセット
    function reset() {
        wasmInstance.exports.hs_reset();
    }

    // アセンブリソースをロード
    function loadAsm(src) {
        const ptr = writeCString(src);
        wasmInstance.exports.hs_load_asm(ptr);
        if (wasmInstance.exports.free) wasmInstance.exports.free(ptr);
    }

    // ── 最小限のWASI実装 ────────────────────────
    function makeWASI() {
        return {
            fd_write(fd, iovs, iovsLen, nwritten) {
                const view = new DataView(memory.buffer);
                let written = 0;
                for (let i = 0; i < iovsLen; i++) {
                    const ptr = view.getUint32(iovs + i*8, true);
                    const len = view.getUint32(iovs + i*8 + 4, true);
                    const str = new TextDecoder().decode(
                        new Uint8Array(memory.buffer, ptr, len));
                    if (fd === 1) process?.stdout?.write?.(str) ?? console.log(str);
                    else          console.error(str);
                    written += len;
                }
                view.setUint32(nwritten, written, true);
                return 0;
            },
            fd_close()     { return 0; },
            fd_seek()      { return 70; },  // EBADF
            proc_exit(code){ throw new Error('WASM exit: ' + code); },
            environ_get()  { return 0; },
            environ_sizes_get(count, size) {
                const v = new DataView(memory.buffer);
                v.setUint32(count, 0, true);
                v.setUint32(size,  0, true);
                return 0;
            },
            clock_time_get(id, precision, time) {
                const v = new DataView(memory.buffer);
                const ns = BigInt(Date.now()) * 1000000n;
                v.setBigUint64(time, ns, true);
                return 0;
            },
        };
    }

    return { init, getLayout, getState, getWires, step, reset, loadAsm };
})();
