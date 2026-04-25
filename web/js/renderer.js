'use strict';
// ============================================================
// Canvas レンダラー
// Haskell から JSON で受け取ったレイアウトを描画する
// ============================================================

class Renderer {
    constructor(canvas) {
        this.canvas  = canvas;
        this.ctx     = canvas.getContext('2d');
        this.camX    = 0;
        this.camY    = 0;
        this.zoom    = 1.0;
        this.layout  = null;   // LayoutComp ツリー (Haskell から受信)
        this.wires   = {};     // wireId → 0/1
        this.prevWires = {};
        this.pulseWires = new Set();   // 変化したワイヤ
        this._dragging = false;
        this._lastX = 0;
        this._lastY = 0;
        this._rafId = null;
        this.onZoomChange = null;
        this._setupEvents();
        this._resize();
    }

    setLayout(layout) {
        this.layout = layout;
        this.fitView();
    }

    updateWires(wireArray) {
        this.prevWires = Object.assign({}, this.wires);
        wireArray.forEach(({ id, v }) => { this.wires[id] = v; });
        // 変化したワイヤをパルスハイライト
        this.pulseWires.clear();
        for (const { id, v } of wireArray) {
            if (this.prevWires[id] !== undefined && this.prevWires[id] !== v)
                this.pulseWires.add(id);
        }
    }

    // ── メインレンダリング ──────────────────────
    render() {
        const ctx  = this.ctx;
        const W    = this.canvas.width;
        const H    = this.canvas.height;
        ctx.fillStyle = '#0a0a0a';
        ctx.fillRect(0, 0, W, H);
        if (!this.layout) {
            ctx.fillStyle = '#888';
            ctx.font = '20px monospace';
            ctx.fillText('Loading CPU...', W/2-80, H/2);
            return;
        }
        ctx.save();
        ctx.translate(-this.camX * this.zoom + W/2, -this.camY * this.zoom + H/2);
        ctx.scale(this.zoom, this.zoom);
        this._drawComp(ctx, this.layout, 0, 0);
        ctx.restore();
    }

    startRenderLoop() {
        const loop = () => {
            this.render();
            this._rafId = requestAnimationFrame(loop);
        };
        loop();
    }

    stopRenderLoop() {
        if (this._rafId) cancelAnimationFrame(this._rafId);
    }

    fitView() {
        if (!this.layout) return;
        const pad = 50;
        const scaleX = (this.canvas.width  - pad*2) / this.layout.w;
        const scaleY = (this.canvas.height - pad*2) / this.layout.h;
        this.zoom = Math.min(scaleX, scaleY, 2);
        this.camX = this.layout.x + this.layout.w / 2;
        this.camY = this.layout.y + this.layout.h / 2;
        this._notifyZoom();
    }

    // ── コンポーネント描画 ───────────────────────
    _drawComp(ctx, comp, absX, absY) {
        const cx = absX + comp.x;
        const cy = absY + comp.y;
        const z  = this.zoom;

        // コンポーネントのスクリーン幅が expandAt(px) 以上になったら展開
        // expandAt=0 は常に展開
        const screenW = comp.w * z;
        const shouldExpand = comp.expandAt === 0 || screenW >= comp.expandAt;

        // コンポーネントの輪郭
        ctx.strokeStyle = comp.color || '#4a9eff';
        ctx.lineWidth = 1 / z;
        ctx.strokeRect(cx, cy, comp.w, comp.h);

        // ラベル (小さいズームでは大きく表示)
        const labelSize = Math.max(4 / z, Math.min(comp.h * 0.15, 12 / z));
        ctx.fillStyle = '#ccc';
        ctx.font = `${labelSize}px monospace`;
        ctx.textAlign = 'center';
        ctx.fillText(comp.label || comp.name, cx + comp.w/2, cy + labelSize + 2/z);
        ctx.textAlign = 'left';

        if (!shouldExpand || z < 0.3) return;

        // ワイヤセグメント
        for (const ws of (comp.wireSegs || [])) {
            this._drawWireSegs(ctx, ws.wireId, ws.segs, cx, cy, z);
        }

        // 子コンポーネント
        for (const child of (comp.children || [])) {
            this._drawComp(ctx, child, cx, cy);
        }

        // ゲート (十分ズームしたとき)
        if (z >= 2) {
            for (const g of (comp.gates || [])) {
                this._drawGate(ctx, g, cx, cy, z);
            }
        }
    }

    _drawWireSegs(ctx, wireId, segs, baseX, baseY, z) {
        const val   = this.wires[wireId] || 0;
        const pulse = this.pulseWires.has(wireId);
        ctx.strokeStyle = pulse ? '#ffff00' : (val ? '#00ff88' : '#334');
        ctx.lineWidth = (pulse ? 1.5 : 0.8) / z;
        ctx.beginPath();
        for (const s of segs) {
            ctx.moveTo(baseX + s.x1, baseY + s.y1);
            ctx.lineTo(baseX + s.x2, baseY + s.y2);
        }
        ctx.stroke();
    }

    _drawGate(ctx, gate, baseX, baseY, z) {
        const gx = baseX + gate.x;
        const gy = baseY + gate.y;
        const val = this.wires[gate.out] || 0;
        ctx.fillStyle = val ? '#00ff88' : '#1e3828';
        ctx.strokeStyle = val ? '#00ff88' : '#4a9eff';
        ctx.lineWidth = 1 / z;
        ctx.beginPath();
        ctx.rect(gx, gy, gate.w, gate.h);
        ctx.fill();
        ctx.stroke();

        const fontSize = gate.h * 0.6;
        if (fontSize * z >= 6) {
            ctx.fillStyle = val ? '#003300' : '#dfe6e9';
            ctx.font = `${fontSize}px monospace`;
            ctx.textAlign = 'center';
            ctx.fillText(gate.type, gx + gate.w/2, gy + gate.h * 0.65);
            ctx.textAlign = 'left';
        }
    }

    // ── イベント ──────────────────────────���──────
    _setupEvents() {
        this.canvas.addEventListener('wheel', e => {
            e.preventDefault();
            const factor = e.deltaY < 0 ? 1.15 : 0.87;
            this.zoom = Math.max(0.1, Math.min(80, this.zoom * factor));
            this._notifyZoom();
        }, { passive: false });

        this.canvas.addEventListener('mousedown', e => {
            this._dragging = true;
            this._lastX = e.clientX;
            this._lastY = e.clientY;
        });
        window.addEventListener('mousemove', e => {
            if (!this._dragging) return;
            this.camX -= (e.clientX - this._lastX) / this.zoom;
            this.camY -= (e.clientY - this._lastY) / this.zoom;
            this._lastX = e.clientX;
            this._lastY = e.clientY;
        });
        window.addEventListener('mouseup', () => { this._dragging = false; });
        window.addEventListener('resize', () => this._resize());
    }

    _resize() {
        this.canvas.width  = window.innerWidth;
        this.canvas.height = window.innerHeight;
    }

    _notifyZoom() {
        if (this.onZoomChange) {
            const desc = this.zoom < 0.5 ? 'Macro View: CPU Blocks'
                       : this.zoom < 2   ? 'Component View'
                       : this.zoom < 8   ? 'Sub-Component View'
                       : this.zoom < 20  ? 'Gate View'
                       : 'Pin-Level View';
            this.onZoomChange(this.zoom, desc);
        }
    }
}
