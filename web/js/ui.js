'use strict';
// ============================================================
// UI ロジック (Haskell WASM を呼び出してCPUを制御する)
// ============================================================

const App = (() => {
    let renderer  = null;
    let running   = false;
    let runTimer  = null;
    let viewBank  = 0;   // どちらのバンクをメモリパネルに表示するか

    // ディスプレイ MMIO アドレス: pixel(row, col) = mem[0xE0 + row*5 + col]
    // row 0: 0xE0-0xE4 / row 1: 0xE5-0xE9 / row 2: 0xEA-0xEE
    // row 3: 0xEF-0xF3 / row 4: 0xF4-0xF8
    const DISPLAY_BASE = 0xE0;
    const KB_DATA_ADDR = 0xDC;   // キーボードデータ MMIO
    const IRQ_VEC_ADDR = 0xFE;   // 割り込みベクタ MMIO

    const PROGRAMS = {
        display: `; Display demo: draw a plus sign on the 5x5 screen
; pixel(row,col) = mem[0xE0 + row*5 + col]  (non-zero = lit)
LOAD_A 1
STORE_A 0xE2  ; row0 col2
STORE_A 0xE7  ; row1 col2
STORE_A 0xEA  ; row2 col0
STORE_A 0xEB  ; row2 col1
STORE_A 0xEC  ; row2 col2
STORE_A 0xED  ; row2 col3
STORE_A 0xEE  ; row2 col4
STORE_A 0xF1  ; row3 col2
STORE_A 0xF6  ; row4 col2
HLT`,
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
        keyboard: `; Keyboard IRQ demo
; キーを押すと mem[0x80] にASCIIコードが格納されます
; ハンドラは byte 0x0A (= 10) から始まります
LOAD_A 10       ; 0x00: ハンドラアドレス = 10 (byte 0x0A)
STORE_A 0xFE    ; 0x02: 割り込みベクタを設定
EI              ; 0x04: 割り込み許可
JMP 6           ; 0x06: メインループ (自己ジャンプで待機)
NOP             ; 0x08: (padding)
; -- IRQハンドラ (byte 0x0A から) --
LOAD_A_MEM 0xDC ; 0x0A: キーコードを読み込む
STORE_A 0x80    ; 0x0C: mem[0x80] に保存
RTI             ; 0x0E: 割り込みから復帰 (IEを再有効化)`,
        life: `; ============================================================
; Conway's Game of Life on the 5x5 display (108 instructions)
;   blinker パターンが水平⇄垂直に振動します。
;   Max Speed モード推奨 (1世代あたり ~4000 ステップ)。
;
; メモリレイアウト:
;   bank 0:
;     0x00..0xD7 : code
;     0xE0..0xF8 : display (現世代)
;   bank 1 (LOAD_A_HIGH / STORE_A_HIGH / LOAD_B_HIGH でアクセス):
;     0x00..0x30 : 7x7 padded grid (border が常に 0 なので
;                   隣接セル読み出しに境界判定が不要)
;     0x40..0x45 : スクラッチ
;       0x40 padded_base   0x41 display_addr   0x42 cells_in_row
;       0x43 count         0x44 offset(引数)   0x45 current/temp
; ============================================================
;
; --- INIT: display に blinker を描き、apply 相にジャンプ
;     (apply が display→padded をコピーするので padded の初期化は不要)
LOAD_A 1            ; 0x00
STORE_A 0xEB        ; display(2,1)
STORE_A 0xEC        ; display(2,2)
STORE_A 0xED        ; display(2,3)
JMP 0x70            ; -> apply_entry
;
; --- main_gen_entry (0x0A): apply tail の JMP 0x0A で戻ってくる
CALL 0x8A           ; reset_state を呼ぶ (padded_base/display_addr/cells_in_row)
;
; --- cell_loop_start (0x0C):
;     1セル分の処理 (隣接 8 マスを数える → ルール → display へ書く)
LOAD_A_HIGH 0x41    ; A = display_addr  (pre-patch トリック)
STORE_A 0x65        ; → wd_op (0x64) のオペランドに焼き付ける
LOAD_A 0
STORE_A_HIGH 0x43   ; count = 0
LOAD_A 0xF8         ; offset -8
STORE_A_HIGH 0x44
CALL 0xC6           ; read_neighbor
LOAD_A 0xF9         ; offset -7
STORE_A_HIGH 0x44
CALL 0xC6
LOAD_A 0xFA         ; offset -6
STORE_A_HIGH 0x44
CALL 0xC6
LOAD_A 0xFF         ; offset -1
STORE_A_HIGH 0x44
CALL 0xC6
LOAD_A 1            ; offset +1
STORE_A_HIGH 0x44
CALL 0xC6
LOAD_A 6            ; offset +6
STORE_A_HIGH 0x44
CALL 0xC6
LOAD_A 7            ; offset +7
STORE_A_HIGH 0x44
CALL 0xC6
LOAD_A 8            ; offset +8
STORE_A_HIGH 0x44
CALL 0xC6
; 現在セルの値を読む (これも自己書換)
LOAD_A_HIGH 0x40    ; A = padded_base
STORE_A 0x49        ; → rc_op (0x48) のオペランドに焼き付ける
LOAD_A_HIGH 0x00    ; 0x48 rc_op: 直前で焼かれたアドレスを読む
STORE_A_HIGH 0x45   ; current = A
; ルール: alive iff count==3 OR (count==2 AND current==1)
LOAD_A_HIGH 0x43    ; 0x4C: A = count
LOAD_B 3
XOR                 ; A = count XOR 3
JZ 0x5E             ; count==3 → alive 分岐
LOAD_B 1
XOR                 ; A = count XOR 2
JNZ 0x62            ; count!=2 → dead 分岐
LOAD_A_HIGH 0x45    ; count==2 の場合: A = current (0 or 1)
JMP 0x64            ; → wd_op
LOAD_A 1            ; 0x5E alive:
JMP 0x64
LOAD_A 0            ; 0x62 dead:
STORE_A 0x00        ; 0x64 wd_op: オペランド焼き済み → display へ書く
; tail: 次セルへ進み、display_addr==0xF9 で apply 相に抜ける
CALL 0x98           ; advance
LOAD_A_HIGH 0x41
LOAD_B 0xF9
XOR
JNZ 0x0C            ; → cell_loop_start
;
; --- apply_entry (0x70): display を padded にコピーして次世代の読み元にする
CALL 0x8A           ; reset_state
;
; --- apply_loop_start (0x72):
LOAD_A_HIGH 0x41    ; A = display_addr
STORE_A 0x7B        ; → ar_op (0x7A) のオペランド
LOAD_A_HIGH 0x40    ; A = padded_base
STORE_A 0x7D        ; → aw_op (0x7C) のオペランド
LOAD_A_MEM 0x00     ; 0x7A ar_op: display[display_addr] を A に
STORE_A_HIGH 0x00   ; 0x7C aw_op: A を padded[padded_base] へ
CALL 0x98           ; advance
LOAD_A_HIGH 0x41
LOAD_B 0xF9
XOR
JNZ 0x72            ; → apply_loop_start
JMP 0x0A            ; → main_gen_entry
;
; --- reset_state (0x8A): 反復前に状態をリセット
LOAD_A 0x08         ; padded_base = 最初の inner cell (padded_index 8)
STORE_A_HIGH 0x40
LOAD_A 0xE0         ; display_addr = 0xE0
STORE_A_HIGH 0x41
LOAD_A 0
STORE_A_HIGH 0x42   ; cells_in_row = 0
RET
;
; --- advance (0x98): display_addr++, cells_in_row++,
;     padded_base は同行で +1, 行末で +3 (左右の border を飛ばす)
LOAD_A_HIGH 0x41
LOAD_B 1
ADD
STORE_A_HIGH 0x41
LOAD_A_HIGH 0x42
LOAD_B 1
ADD
STORE_A_HIGH 0x42
LOAD_B 5
SUB
JZ 0xB8             ; cells_in_row == 5 → new_row
LOAD_A_HIGH 0x40
LOAD_B 1
ADD
STORE_A_HIGH 0x40
RET
LOAD_A 0            ; 0xB8 new_row:
STORE_A_HIGH 0x42
LOAD_A_HIGH 0x40
LOAD_B 3
ADD                 ; padded_base += 3 (right border, left border, +1)
STORE_A_HIGH 0x40
RET
;
; --- read_neighbor (0xC6):
;     padded_base + offset の値を count に加算する
LOAD_A_HIGH 0x40
LOAD_B_HIGH 0x44
ADD
STORE_A 0xCF        ; → nb_op (0xCE) のオペランド
LOAD_A_HIGH 0x00    ; 0xCE nb_op: padded[padded_base + offset] を A に
LOAD_B_HIGH 0x43    ; B = count
ADD
STORE_A_HIGH 0x43   ; count = count + neighbor
RET`,
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

        // IRQ ステータス
        const irqEl = document.getElementById('irqStatus');
        if (irqEl) {
            const ie = !!state.irqEnabled;
            const ip = !!state.irqPending;
            irqEl.textContent = 'IRQ: IE=' + (ie?'1':'0') + ' IP=' + (ip?'1':'0');
            irqEl.className = 'irq-status' + (ip ? ' irq-pending' : '') + (ie ? ' irq-enabled' : '');
        }

        // ステップボタンを非活性化
        const btnStep = document.getElementById('btnStep');
        const btnRun  = document.getElementById('btnRun');
        if (btnStep) btnStep.disabled = !!state.halted;
        if (btnRun)  btnRun.disabled  = !!state.halted || running;

        // メモリビュー
        updateMemoryView(state.memory || [], state);
    }

    function updateMemoryView(mem, state) {
        const el = document.getElementById('memoryView');
        if (!el) return;
        const pcVal  = (state && state.pcVal)  || 0;
        const spVal  = (state && state.spVal)  || 0;
        const base   = viewBank * 256;

        const rows = [];
        for (let i = 0; i < 256; i += 8) {
            const addrStr = '0x' + (base + i).toString(16).padStart(3,'0');
            const cells = [];
            for (let j = 0; j < 8; j++) {
                const a = i + j;
                const v = mem[base + a] || 0;
                const hex = v.toString(16).padStart(2,'0');
                const tip = '0x' + (base + a).toString(16).padStart(3,'0') + '=' + v;
                let cls = 'mem-cell';
                // PC / SP / MMIO は bank 0 のみで意味を持つ
                if (viewBank === 0) {
                    if (a === pcVal || a === pcVal + 1) cls += ' mem-pc';
                    else if (a === spVal)               cls += ' mem-sp';
                    else if (a === KB_DATA_ADDR)        cls += ' mem-kb';
                    else if (a === IRQ_VEC_ADDR)        cls += ' mem-vec';
                    else if (a >= DISPLAY_BASE && a < DISPLAY_BASE + 25) cls += ' mem-dsp';
                    else if (v !== 0)                   cls += ' mem-nz';
                } else if (v !== 0)                     cls += ' mem-nz';
                cells.push(`<span class="${cls}" title="${tip}">${hex}</span>`);
            }
            rows.push(
                `<div class="mem-row">` +
                `<span class="mem-addr">${addrStr}</span>` +
                `<span class="mem-cells">${cells.join('')}</span>` +
                `</div>`
            );
        }
        el.innerHTML = rows.join('');

        // PC のある行が見えるようにスクロール (bank 0 のみ)
        if (viewBank === 0) {
            const pcCell = el.querySelector('.mem-pc');
            if (pcCell) pcCell.scrollIntoView({ block: 'nearest' });
        }
    }

    function stopRunning() {
        running = false;
        if (runTimer) { clearInterval(runTimer); clearTimeout(runTimer); runTimer = null; }
        document.getElementById('btnRun')?.setAttribute('disabled', false);
        document.getElementById('btnPause')?.setAttribute('disabled', true);
        // 最高速モードの display-only ループを止めて、フル描画に戻す
        if (renderer) {
            renderer.stopRenderLoop();
            try { renderer.updateWires(HaskellCPU.getWires()); } catch (e) {}
            renderer.startRenderLoop();
        }
        updateUI();
    }

    // 通常モード: speedSlider に応じた間隔で1ステップずつ実行
    function startNormalRun() {
        const speed = parseInt(document.getElementById('speedSlider')?.value || 5);
        const delay = Math.max(50, 1100 - speed * 100);
        runTimer = setInterval(() => {
            let state = {};
            try { state = HaskellCPU.getState(); } catch (e) {}
            if (state.halted || !running) { stopRunning(); return; }
            step();
        }, delay);
    }

    // 最高速モード: 回路ビューと UI 更新は省略。display だけ rAF で更新する。
    function startMaxSpeedRun() {
        if (renderer) {
            renderer.stopRenderLoop();
            // display-only ループ: 各 rAF tick で最新のワイヤを取り直して描く
            renderer.startDisplayOnlyLoop(() => {
                try {
                    renderer.updateWires(HaskellCPU.getWires());
                    // 最高速モードでは大量に状態が変わるので pulse(黄色) は出さない
                    renderer.pulseWires.clear();
                } catch (e) {}
            });
        }
        const BATCH = 2000;
        const tick = () => {
            if (!running) return;
            const start = performance.now();
            let halted = false;
            while (performance.now() - start < 16) {
                for (let i = 0; i < BATCH; i++) {
                    try { HaskellCPU.step(); }
                    catch (e) { halted = true; break; }
                }
                if (halted) break;
                try { if (HaskellCPU.getState().halted) { halted = true; break; } }
                catch (e) { halted = true; break; }
            }
            if (halted) { stopRunning(); return; }
            runTimer = setTimeout(tick, 0);
        };
        tick();
    }

    // ── パネル ドラッグ移動 ─────────────────────
    function makePanelDraggable(panel) {
        const header = panel.querySelector('h2');
        if (!header) return;
        let preventClick = false;

        // ドラッグ後のクリック（折りたたみトリガー）を1回だけ抑止
        header.addEventListener('click', e => {
            if (preventClick) { e.stopImmediatePropagation(); preventClick = false; }
        }, true);

        header.addEventListener('mousedown', e => {
            if (e.button !== 0) return;
            const startX = e.clientX, startY = e.clientY;
            const rect   = panel.getBoundingClientRect();
            const ox = e.clientX - rect.left;
            const oy = e.clientY - rect.top;
            let moved = false;

            function onMove(me) {
                if (!moved) {
                    if (Math.hypot(me.clientX - startX, me.clientY - startY) < 5) return;
                    moved = true;
                    panel.style.bottom = 'auto';
                    panel.style.right  = 'auto';
                    panel.style.left   = rect.left + 'px';
                    panel.style.top    = rect.top  + 'px';
                    me.preventDefault();
                }
                const nx = Math.max(0, Math.min(window.innerWidth  - 40, me.clientX - ox));
                const ny = Math.max(0, Math.min(window.innerHeight - 20, me.clientY - oy));
                panel.style.left = nx + 'px';
                panel.style.top  = ny + 'px';
            }
            function onUp() {
                document.removeEventListener('mousemove', onMove);
                document.removeEventListener('mouseup',  onUp);
                if (moved) preventClick = true;
            }
            document.addEventListener('mousemove', onMove);
            document.addEventListener('mouseup',   onUp);
        });
    }

    // ── メモリパネル Ctrl+ホイール ズーム ───────
    function setupMemoryZoom() {
        const panel = document.getElementById('memoryPanel');
        const view  = document.getElementById('memoryView');
        if (!panel || !view) return;
        panel.addEventListener('wheel', e => {
            if (!e.ctrlKey) return;
            e.preventDefault();
            const cur  = parseFloat(getComputedStyle(view).fontSize) || 11;
            const next = Math.max(8, Math.min(18, cur + (e.deltaY < 0 ? 1 : -1)));
            view.style.fontSize = next + 'px';
        }, { passive: false });
    }

    // ── UI セットアップ ────────────────────────
    function setupUI() {
        document.getElementById('btnStep')?.addEventListener('click', () => step());

        document.getElementById('btnRun')?.addEventListener('click', () => {
            if (running) return;
            running = true;
            document.getElementById('btnPause').disabled = false;
            document.getElementById('btnRun').disabled   = true;
            if (document.getElementById('maxSpeed')?.checked) startMaxSpeedRun();
            else                                              startNormalRun();
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
            if (running && !document.getElementById('maxSpeed')?.checked) {
                stopRunning();
                document.getElementById('btnRun').click();
            }
        });

        // 最高速モード切り替え: スライダーを無効化し、実行中なら新モードで再開
        document.getElementById('maxSpeed')?.addEventListener('change', e => {
            const on = !!e.target.checked;
            const slider = document.getElementById('speedSlider');
            if (slider) slider.disabled = on;
            if (running) {
                stopRunning();
                document.getElementById('btnRun').click();
            }
        });

        document.getElementById('btnHelp')?.addEventListener('click', () => {
            document.getElementById('helpOverlay').style.display = 'flex';
        });

        document.getElementById('btnOpenCompiler')?.addEventListener('click', () => {
            window.open('compiler.html', 'compiler',
                'width=1100,height=700,menubar=no,toolbar=no,location=no');
        });

        // コンパイラウィンドウから受信した ASM をロード
        window.addEventListener('message', e => {
            if (e.data && e.data.type === 'loadAsm' && e.data.code) {
                const src = e.data.code;
                try {
                    HaskellCPU.reset();
                    HaskellCPU.loadAsm(src);
                    renderer.updateWires(HaskellCPU.getWires());
                    updateUI();
                    // カスタムプログラムとして表示
                    document.getElementById('customProgram').value = src;
                    document.getElementById('programSelect').value = 'custom';
                    document.getElementById('customProgram').style.display = 'block';
                    updateProgramListing(src);
                } catch(err) { console.warn('loadAsm from compiler:', err); }
            }
        });

        document.addEventListener('keydown', e => {
            if (e.target.tagName === 'TEXTAREA') return;
            // UI ホットキー
            if (e.code === 'Space') { e.preventDefault(); step(); return; }
            if (e.code === 'KeyR' && !e.ctrlKey && !e.metaKey) { e.preventDefault(); document.getElementById('btnRun')?.click(); return; }
            if (e.code === 'KeyP') { e.preventDefault(); stopRunning(); return; }
            // 印字可能なキーを IRQ として CPU に送る
            if (e.key.length === 1) {
                const ascii = e.key.charCodeAt(0);
                try { HaskellCPU.setIRQ(ascii); } catch (_) {}
            }
        });

        // Memory パネルの Bank 切替
        document.querySelectorAll('.bank-btn').forEach(btn => {
            btn.addEventListener('click', () => {
                viewBank = parseInt(btn.dataset.bank) | 0;
                document.querySelectorAll('.bank-btn').forEach(b =>
                    b.classList.toggle('bank-btn-active', parseInt(b.dataset.bank) === viewBank));
                updateUI();
            });
        });

        // 全パネルをドラッグ可能にする
        document.querySelectorAll('.panel').forEach(makePanelDraggable);
        // メモリパネルの Ctrl+ホイール ズーム
        setupMemoryZoom();
    }

    return { init };
})();

window.addEventListener('load', () => App.init());
