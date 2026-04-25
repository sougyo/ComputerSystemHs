'use strict';
// ============================================================
// Haskell/WASM ブリッジ
//
// GHC WASM バックエンドが必要とする WASI preview1 関数を
// すべて実装し、_start() で Haskell ランタイムを起動する。
// ============================================================

// proc_exit() で投げる専用シグナル (エラーではない)
class ExitSignal extends Error {
    constructor(code) { super('ExitSignal'); this.exitCode = code; }
}

const HaskellCPU = (() => {
    let inst   = null;   // WebAssembly.Instance
    let memory = null;   // WebAssembly.Memory

    // ── メモリ読み書きヘルパー ───────────────────
    const u8  = () => new Uint8Array(memory.buffer);
    const dv  = () => new DataView(memory.buffer);

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

    // ── WASI preview1 完全実装 ───────────────────
    // GHC WASM が必要とする 20 関数をすべて実装する。
    // ファイルシステム系は「存在しない」として返す。
    function makeWASI() {
        const ESUCCESS = 0;
        const EBADF    = 8;
        const ENOSYS   = 52;

        function write_u32(ptr, val) { dv().setUint32(ptr, val, true); }
        function write_u64(ptr, val) { dv().setBigUint64(ptr, BigInt(val), true); }

        return {
            // ── 引数・環境 ──────────────────────
            args_get(argv, argvBuf) {
                write_u32(argv, 0);
                return ESUCCESS;
            },
            args_sizes_get(argc, argvBufSize) {
                write_u32(argc, 0);
                write_u32(argvBufSize, 0);
                return ESUCCESS;
            },
            environ_get(environ, environBuf) {
                write_u32(environ, 0);
                return ESUCCESS;
            },
            environ_sizes_get(count, size) {
                write_u32(count, 0);
                write_u32(size, 0);
                return ESUCCESS;
            },

            // ── 時計 ────────────────────────────
            clock_time_get(clockId, precision, timePtr) {
                const ns = BigInt(Date.now()) * 1_000_000n;
                dv().setBigUint64(timePtr, ns, true);
                return ESUCCESS;
            },

            // ── ファイル記述子 ───────────────────
            fd_close(fd) { return ESUCCESS; },

            fd_fdstat_get(fd, statPtr) {
                const v = dv();
                // filetype: character_device(2) for stdio, unknown(0) for others
                v.setUint8(statPtr,      fd <= 2 ? 2 : 0);
                v.setUint8(statPtr + 1,  0);
                v.setUint16(statPtr + 2, 0, true);   // fdflags
                v.setBigUint64(statPtr + 8,  0n, true); // rights_base
                v.setBigUint64(statPtr + 16, 0n, true); // rights_inheriting
                return ESUCCESS;
            },
            fd_fdstat_set_flags(fd, flags) { return ESUCCESS; },

            fd_filestat_get(fd, statPtr) {
                // ゼロ埋め (空のファイル統計)
                new Uint8Array(memory.buffer, statPtr, 64).fill(0);
                return ESUCCESS;
            },
            fd_filestat_set_size(fd, size) { return EBADF; },

            // 事前オープンディレクトリなし
            fd_prestat_get(fd, prestatPtr) { return EBADF; },
            fd_prestat_dir_name(fd, pathPtr, pathLen) { return EBADF; },

            fd_read(fd, iovsPtr, iovsLen, nreadPtr) {
                write_u32(nreadPtr, 0);
                return ESUCCESS;
            },

            fd_seek(fd, offset, whence, newOffsetPtr) {
                dv().setBigUint64(newOffsetPtr, 0n, true);
                return ESUCCESS;
            },

            fd_write(fd, iovsPtr, iovsLen, nwrittenPtr) {
                const view = dv();
                const mem  = u8();
                let written = 0;
                for (let i = 0; i < iovsLen; i++) {
                    const ptr = view.getUint32(iovsPtr + i * 8,     true);
                    const len = view.getUint32(iovsPtr + i * 8 + 4, true);
                    const text = new TextDecoder().decode(mem.subarray(ptr, ptr + len));
                    if (fd === 1) console.log('[Haskell]', text.trimEnd());
                    else          console.warn('[Haskell stderr]', text.trimEnd());
                    written += len;
                }
                write_u32(nwrittenPtr, written);
                return ESUCCESS;
            },

            // ── ファイルシステム (未対応) ────────
            path_create_directory(fd, pathPtr, pathLen) { return EBADF; },
            path_filestat_get(fd, flags, pathPtr, pathLen, statPtr) { return EBADF; },
            path_open(fd, dirflags, pathPtr, pathLen, oflags,
                      fsRightsBase, fsRightsInheriting, fdflags, openedFdPtr) {
                return EBADF;
            },

            // ── イベント ────────────────────────
            poll_oneoff(inPtr, outPtr, nsubs, neventsPtr) {
                write_u32(neventsPtr, 0);
                return ESUCCESS;
            },

            // ── プロセス終了 ─────────────────────
            // 専用例外を投げて _start() を止める。
            // これにより GHC ランタイムのクリーンアップが走らず、
            // WASM メモリ上の Haskell 状態が保たれる。
            proc_exit(code) {
                throw new ExitSignal(code);
            },
        };
    }

    // ── 初期化 ───────────────────────────────────
    async function init(wasmPath) {
        const response = await fetch(wasmPath);
        if (!response.ok) throw new Error(`fetch failed: ${response.status} ${wasmPath}`);

        const buffer = await response.arrayBuffer();

        // memory は instantiate 後にセットするので lazy 参照
        const imports = { wasi_snapshot_preview1: makeWASI() };

        const result = await WebAssembly.instantiate(buffer, imports);
        inst   = result.instance;
        memory = inst.exports.memory;

        // ビルド方式: -no-hs-main + src/wasm_stub.c
        //
        // _start() は C の main() (return 0) を呼ぶだけで GHC hs_main を
        // 経由しないため hs_exit() が呼ばれず、proc_exit も来ない。
        // _start() 完了後に hs_init(0,0) を呼ぶと GHC RTS が初期化され、
        // 以降の hs_* エクスポートが正常に動作する。
        if (typeof inst.exports._start === 'function') {
            try {
                inst.exports._start();
            } catch (e) {
                if (!(e instanceof ExitSignal)) throw e;
                console.log(`[Haskell] _start ExitSignal (exit code: ${e.exitCode})`);
            }
        }

        // GHC RTS を明示的に初期化する
        if (typeof inst.exports.hs_init === 'function') {
            inst.exports.hs_init(0, 0);
            console.log('[Haskell] hs_init 完了 (RTS running)');
        } else {
            throw new Error('WASM: hs_init が見つかりません。make wasm でリビルドしてください。');
        }

        return true;
    }

    // ── 公開 API ─────────────────────────────────

    function getLayout() {
        const ptr = inst.exports.hs_get_layout();
        return JSON.parse(readCString(ptr));
    }

    function getState() {
        const ptr = inst.exports.hs_get_state();
        return JSON.parse(readCString(ptr));
    }

    function getWires() {
        const ptr = inst.exports.hs_get_wires();
        return JSON.parse(readCString(ptr));
    }

    function step()  { inst.exports.hs_step();  }
    function reset() { inst.exports.hs_reset(); }

    function loadAsm(src) {
        const ptr = writeCString(src);
        inst.exports.hs_load_asm(ptr);
        inst.exports.free(ptr);
    }

    return { init, getLayout, getState, getWires, step, reset, loadAsm };
})();
