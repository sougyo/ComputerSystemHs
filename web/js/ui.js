'use strict';
// ============================================================
// UI ロジック (Haskell WASM を呼び出してCPUを制御する)
// ============================================================

const App = (() => {
    let renderer  = null;
    let running   = false;
    let runTimer  = null;

    const PROGRAMS = {
        add: `; Addition: A = 25 + 17 = 42
LOAD_A 25
LOAD_B 17
ADD
HLT`,
        count: `; Count up to 10 in A
LOAD_A 0
LOAD_B 1
ADD
STORE_A 0x80
LOAD_B 10
SUB
JZ 20
LOAD_A_MEM 0x80
LOAD_B 1
JMP 4
HLT`,
        logic: `; Logic operations
LOAD_A 0xCC
LOAD_B 0xAA
AND
STORE_A 0x80
LOAD_A 0xCC
OR
STORE_A 0x81
LOAD_A 0xCC
XOR
STORE_A 0x82
NOT
STORE_A 0x83
HLT`,
        fibonacci: `; Fibonacci sequence (simplified)
LOAD_A 1
STORE_A 0x80
LOAD_A 1
STORE_A 0x81
LOAD_A_MEM 0x80
LOAD_B 0
LOAD_A_MEM 0x81
STORE_A 0x80
HLT`,
    };

    // ── 初期化 ───────────────���─────────────────
    async function init() {
        const canvas = document.getElementById('mainCanvas');
        renderer = new Renderer(canvas);
        renderer.onZoomChange = (zoom, desc) => {
            document.getElementById('zoomInfo').textContent = zoom.toFixed(2) + 'x';
            document.getElementById('zoomHint').textContent = desc;
        };
        renderer.startRenderLoop();

        // WASM のロードを試みる
        let wasmLoaded = false;
        try {
            wasmLoaded = await HaskellCPU.init('computersystem.wasm');
        } catch (e) {
            console.warn('WASM not available, running in demo mode:', e.message);
        }

        if (wasmLoaded) {
            const layout = HaskellCPU.getLayout();
            renderer.setLayout(layout);
            updateUI();
        } else {
            // デモモード: WASMなしでレイアウトのみ表示
            showWasmNotice();
        }

        setupUI();
        loadProgram('add');
    }

    function showWasmNotice() {
        const notice = document.getElementById('wasmNotice');
        if (notice) notice.style.display = 'block';
    }

    // ── プログラム管理 ──────────────────────────
    function loadProgram(name) {
        const src = name === 'custom'
            ? document.getElementById('customProgram').value
            : (PROGRAMS[name] || '');
        document.getElementById('customProgram').value = src;
        try {
            HaskellCPU.reset();
            HaskellCPU.loadAsm(src);
            renderer.updateWires(HaskellCPU.getWires());
        } catch (e) {
            console.warn('loadProgram:', e);
        }
        updateUI();
        updateProgramListing(src);
    }

    function updateProgramListing(src) {
        const el = document.getElementById('programListing');
        if (!el) return;
        const lines = src.split('\n').filter(l => l.trim() && !l.trim().startsWith(';'));
        el.innerHTML = lines.map((l, i) =>
            `<div class="prog-line"><span class="addr">0x${(i*2).toString(16).padStart(2,'0')}</span> ${escHtml(l)}</div>`
        ).join('');
    }

    function escHtml(s) {
        return s.replace(/&/g,'&amp;').replace(/</g,'&lt;').replace(/>/g,'&gt;');
    }

    // ── CPU実行 ─────────────────���──────────────
    function step() {
        try {
            HaskellCPU.step();
            renderer.updateWires(HaskellCPU.getWires());
        } catch (e) { console.warn('step:', e); }
        updateUI();
    }

    function updateUI() {
        let state = {};
        try { state = HaskellCPU.getState(); } catch (e) { return; }

        const v = (id, val) => {
            const el = document.getElementById(id);
            if (el) el.textContent = val;
        };
        const toBin = n => n.toString(2).padStart(8, '0');

        v('regA',     state.regA  || '0x00');
        v('regA-bin', toBin(state.regAVal || 0));
        v('regB',     state.regB  || '0x00');
        v('regB-bin', toBin(state.regBVal || 0));
        v('regPC',    state.pc    || '0x00');
        v('regPC-bin',toBin(state.pcVal || 0));
        v('regIR',    (state.irOpcode || '0x00') + ' ' + (state.irOperand || '0x00'));

        const fz = document.getElementById('flagZ');
        const fc = document.getElementById('flagC');
        const fn = document.getElementById('flagN');
        if (fz) { fz.textContent = 'Z=' + (state.flagZ ? '1' : '0'); fz.classList.toggle('active', !!state.flagZ); }
        if (fc) { fc.textContent = 'C=' + (state.flagC ? '1' : '0'); fc.classList.toggle('active', !!state.flagC); }
        if (fn) { fn.textContent = 'N=' + (state.flagN ? '1' : '0'); fn.classList.toggle('active', !!state.flagN); }

        v('currentInstr', state.instrName || '---');
        v('currentPhase', state.halted ? 'HALTED' : 'READY');

        // ステップボタンを非活性化
        const btnStep = document.getElementById('btnStep');
        const btnRun  = document.getElementById('btnRun');
        if (btnStep) btnStep.disabled = !!state.halted;
        if (btnRun)  btnRun.disabled  = !!state.halted || running;

        // メモリビュー
        updateMemoryView(state.memory || []);
    }

    function updateMemoryView(mem) {
        const el = document.getElementById('memoryView');
        if (!el) return;
        const rows = [];
        for (let i = 0; i < 64; i += 8) {
            const addr = '0x' + i.toString(16).padStart(2,'0');
            const bytes = mem.slice(i, i+8).map(b =>
                (b||0).toString(16).padStart(2,'0')).join(' ');
            rows.push(`<div class="mem-row"><span class="mem-addr">${addr}</span> <span class="mem-bytes">${bytes}</span></div>`);
        }
        el.innerHTML = rows.join('');
    }

    function stopRunning() {
        running = false;
        if (runTimer) { clearInterval(runTimer); runTimer = null; }
        document.getElementById('btnRun')?.setAttribute('disabled', false);
        document.getElementById('btnPause')?.setAttribute('disabled', true);
    }

    // ── UI セットアップ ────────────────────────
    function setupUI() {
        document.getElementById('btnStep')?.addEventListener('click', () => step());

        document.getElementById('btnRun')?.addEventListener('click', () => {
            if (running) return;
            running = true;
            document.getElementById('btnPause').disabled = false;
            document.getElementById('btnRun').disabled   = true;
            const speed = parseInt(document.getElementById('speedSlider')?.value || 5);
            const delay = Math.max(50, 1100 - speed * 100);
            runTimer = setInterval(() => {
                let state = {};
                try { state = HaskellCPU.getState(); } catch (e) {}
                if (state.halted || !running) { stopRunning(); updateUI(); return; }
                step();
            }, delay);
        });

        document.getElementById('btnPause')?.addEventListener('click', stopRunning);

        document.getElementById('btnReset')?.addEventListener('click', () => {
            stopRunning();
            const sel = document.getElementById('programSelect')?.value || 'add';
            loadProgram(sel);
        });

        document.getElementById('btnFitView')?.addEventListener('click', () => {
            renderer?.fitView();
        });

        document.getElementById('btnLoad')?.addEventListener('click', () => {
            const sel = document.getElementById('programSelect')?.value || 'add';
            loadProgram(sel);
        });

        document.getElementById('programSelect')?.addEventListener('change', e => {
            const v = e.target.value;
            const custArea = document.getElementById('customProgram');
            if (custArea) custArea.style.display = v === 'custom' ? 'block' : 'none';
        });

        document.getElementById('speedSlider')?.addEventListener('input', e => {
            document.getElementById('speedLabel').textContent = e.target.value;
            if (running) {
                stopRunning();
                document.getElementById('btnRun').click();
            }
        });

        document.getElementById('btnHelp')?.addEventListener('click', () => {
            document.getElementById('helpOverlay').style.display = 'flex';
        });

        document.addEventListener('keydown', e => {
            if (e.target.tagName === 'TEXTAREA') return;
            if (e.code === 'Space') { e.preventDefault(); step(); }
            if (e.code === 'KeyR') { e.preventDefault(); document.getElementById('btnRun')?.click(); }
            if (e.code === 'KeyP') { e.preventDefault(); stopRunning(); }
        });
    }

    return { init };
})();

window.addEventListener('load', () => App.init());
