'use strict';
// ============================================================
// Haskell/WASM ブリッジ (コンパイラ専用)
//
// エクスポート:
//   hs_compile(src)    → asm text (or "ERROR: ...")
//   hs_parse_ast(src)  → AST Show string (or "ERROR: ...")
// ============================================================

class ExitSignal extends Error {
    constructor(code) { super('ExitSignal'); this.exitCode = code; }
}

const HaskellCompiler = (() => {
    let inst   = null;
    let memory = null;

    const u8 = () => new Uint8Array(memory.buffer);
    const dv = () => new DataView(memory.buffer);

    function readCString(ptr) {
        const buf = u8();
        let end = ptr;
        while (buf[end] !== 0) end++;
        return new TextDecoder().decode(buf.subarray(ptr, end));
    }

    function writeCString(str) {
        const bytes = new TextEncoder().encode(str + '\0');
        const ptr   = inst.exports.malloc(bytes.length);
        u8().set(bytes, ptr);
        return ptr;
    }

    function makeWASI() {
        const ESUCCESS = 0, EBADF = 8;
        function w32(ptr, val) { dv().setUint32(ptr, val, true); }
        function w64(ptr, val) { dv().setBigUint64(ptr, BigInt(val), true); }
        return {
            args_get(a, b)             { w32(a, 0); return ESUCCESS; },
            args_sizes_get(c, s)       { w32(c, 0); w32(s, 0); return ESUCCESS; },
            environ_get(e, b)          { w32(e, 0); return ESUCCESS; },
            environ_sizes_get(c, s)    { w32(c, 0); w32(s, 0); return ESUCCESS; },
            clock_time_get(id, p, ptr) {
                dv().setBigUint64(ptr, BigInt(Date.now()) * 1_000_000n, true);
                return ESUCCESS;
            },
            fd_close(fd)               { return ESUCCESS; },
            fd_fdstat_get(fd, p) {
                const v = dv();
                v.setUint8(p, fd <= 2 ? 2 : 0);
                v.setUint8(p + 1, 0);
                v.setUint16(p + 2, 0, true);
                v.setBigUint64(p + 8,  0n, true);
                v.setBigUint64(p + 16, 0n, true);
                return ESUCCESS;
            },
            fd_fdstat_set_flags()      { return ESUCCESS; },
            fd_filestat_get(fd, p)     { new Uint8Array(memory.buffer, p, 64).fill(0); return ESUCCESS; },
            fd_filestat_set_size()     { return EBADF; },
            fd_prestat_get()           { return EBADF; },
            fd_prestat_dir_name()      { return EBADF; },
            fd_read(fd, iv, n, nr)     { w32(nr, 0); return ESUCCESS; },
            fd_seek(fd, o, w, np)      { dv().setBigUint64(np, 0n, true); return ESUCCESS; },
            fd_write(fd, iv, n, nw) {
                const view = dv(), mem = u8();
                let written = 0;
                for (let i = 0; i < n; i++) {
                    const ptr = view.getUint32(iv + i * 8, true);
                    const len = view.getUint32(iv + i * 8 + 4, true);
                    const txt = new TextDecoder().decode(mem.subarray(ptr, ptr + len));
                    if (fd === 1) console.log('[Compiler]', txt.trimEnd());
                    else          console.warn('[Compiler stderr]', txt.trimEnd());
                    written += len;
                }
                w32(nw, written);
                return ESUCCESS;
            },
            path_create_directory()    { return EBADF; },
            path_filestat_get()        { return EBADF; },
            path_open()                { return EBADF; },
            poll_oneoff(i, o, n, ne)   { w32(ne, 0); return ESUCCESS; },
            proc_exit(code)            { throw new ExitSignal(code); },
        };
    }

    async function init(wasmPath) {
        const response = await fetch(wasmPath);
        if (!response.ok) throw new Error(`fetch failed: ${response.status} ${wasmPath}`);
        const buffer  = await response.arrayBuffer();
        const imports = { wasi_snapshot_preview1: makeWASI() };
        const result  = await WebAssembly.instantiate(buffer, imports);
        inst   = result.instance;
        memory = inst.exports.memory;

        if (typeof inst.exports._start === 'function') {
            try { inst.exports._start(); }
            catch (e) { if (!(e instanceof ExitSignal)) throw e; }
        }
        if (typeof inst.exports.hs_init === 'function') {
            inst.exports.hs_init(0, 0);
        } else {
            throw new Error('compiler.wasm: hs_init が見つかりません。make wasm-compiler でビルドしてください。');
        }
        return true;
    }

    function compile(src) {
        const ptr = writeCString(src);
        const ret = inst.exports.hs_compile(ptr);
        inst.exports.free(ptr);
        return readCString(ret);
    }

    function parseAst(src) {
        const ptr = writeCString(src);
        const ret = inst.exports.hs_parse_ast(ptr);
        inst.exports.free(ptr);
        return readCString(ret);
    }

    return { init, compile, parseAst };
})();
