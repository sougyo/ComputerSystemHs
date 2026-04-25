# ComputerSystem Haskell

8ビット CPU のゲートレベルシミュレーター。  
論理回路シミュレーション部分を**純粋 Haskell** で実装し、**GHC WebAssembly バックエンド**でブラウザ上で動作させる。

---

## 特徴

- **純粋関数によるシミュレーション** — 論理ゲートの評価は副作用のない純粋な関数として実装
- **ゲートレベルの忠実な再現** — AND/OR/NAND/XOR/NOT/BUF の 689 ゲートで CPU を構成
- **階層コンポーネント** — SR ラッチ → D ラッチ → DFF → 8bit レジスタ → ALU → CPU
- **WebAssembly 対応** — GHC WASM バックエンドでコンパイルしてブラウザで実行
- **外部パッケージ不依存** — GHC 付属パッケージ (`base`, `containers`, `array`, `mtl`) のみ使用

---

## アーキテクチャ

```
Haskell (純粋シミュレーション)
  ↕ WASM exports (hs_step / hs_get_state 等)
JavaScript (Canvas レンダリング + UI)
```

### 命令セット (16 命令)

| オペコード | ニーモニック  | 動作                  |
|-----------|-------------|-----------------------|
| `0x00`    | `NOP`       | 何もしない              |
| `0x01`    | `LOAD_A n`  | A ← n                 |
| `0x02`    | `LOAD_B n`  | B ← n                 |
| `0x03`    | `LOAD_A_MEM addr` | A ← MEM[addr]  |
| `0x04`    | `STORE_A addr`    | MEM[addr] ← A  |
| `0x05`    | `ADD`       | A ← A + B             |
| `0x06`    | `SUB`       | A ← A − B             |
| `0x07`    | `AND`       | A ← A & B             |
| `0x08`    | `OR`        | A ← A \| B            |
| `0x09`    | `XOR`       | A ← A ^ B             |
| `0x0A`    | `NOT`       | A ← ~A                |
| `0x0B`    | `JMP addr`  | PC ← addr             |
| `0x0C`    | `JZ addr`   | Z=1 なら PC ← addr    |
| `0x0D`    | `JNZ addr`  | Z=0 なら PC ← addr    |
| `0x0E`    | `SHL`       | A ← A << 1, C ← 元bit7 |
| `0x0F`    | `HLT`       | 停止                   |

---

## ファイル構成

```
ComputerSystemHs/
├── src/
│   ├── Circuit.hs          # 論理ゲートの型と純粋評価エンジン
│   ├── Builder.hs          # Builder モナド・トポロジカルソート
│   ├── Assembler.hs        # テキストアセンブリ → バイト列
│   ├── SimpleJSON.hs       # JSON 生成 (外部ライブラリ不要)
│   ├── Serialize.hs        # CPU状態・レイアウト → JSON
│   ├── Main.hs             # WASM エクスポート / ネイティブ実行エントリ
│   └── CPU/
│       ├── Types.hs        # CPUState, CPURefs, DecoderRefs
│       ├── Components.hs   # コンポーネントビルダー (SRラッチ〜CPU全体)
│       └── Step.hs         # stepCPU (純粋関数)
├── web/
│   ├── index.html
│   ├── css/style.css
│   └── js/
│       ├── wasm-bridge.js  # WASM ロード・関数呼び出しブリッジ
│       ├── renderer.js     # Canvas ズーム/パン レンダラー
│       └── ui.js           # UI ロジック
├── computersystem.cabal
└── Makefile
```

---

## 設計: 純粋 Haskell による回路シミュレーション

### 型

```haskell
-- ワイヤの値は IntMap で管理 (副作用なし)
type WireVals = IntMap Bool

-- ゲートは入出力ワイヤIDの純粋なデータ
data Gate = Gate
  { gType :: !GateType   -- AND | OR | NAND | ...
  , gIns  :: ![Int]      -- 入力ワイヤID
  , gOut  :: !Int        -- 出力ワイヤID
  }
```

### 評価

```haskell
-- 1パス: トポロジカル順に全ゲートを評価 (純粋)
evalOnePass :: [Gate] -> WireVals -> WireVals

-- フリップフロップのフィードバックに対応: 安定するまで繰り返す
evaluate :: [Gate] -> WireVals -> WireVals
evaluate gs = go 20
  where
    go 0 vs = vs
    go n vs = let vs' = evalOnePass gs vs
              in  if vs' == vs then vs' else go (n-1) vs'
```

### CPU ステップ (純粋関数)

```haskell
stepCPU :: [Gate] -> CPURefs -> CPUState -> CPUState
-- Gates と CPURefs は起動時に1度だけ構築 (不変)
-- CPUState だけが各ステップで更新される (イミュータブル更新)
```

### Builder モナド

コンポーネント構築は State モナドで副作用なく行う:

```haskell
buildHalfAdder :: String -> Double -> Double -> Int -> Int
               -> Build (LayoutComp, Int, Int)
--                        ^          ^Sum  ^Carry
--                        レイアウト情報

buildCPU :: CPUBuildResult  -- 純粋値として構築完了
```

JS 版のような「後から配線を書き換える」パターンを避け、  
**入力ワイヤIDをパラメータとして渡す**ことで純粋に構築する。

---

## ビルドと実行

### 必要なもの

- GHC 9.4 以上 (Ubuntu: `sudo apt install ghc`)
- cabal (Ubuntu: `sudo apt install cabal-install`)
- WASM ビルドには GHC WASM バックエンド (後述)

### ネイティブ実行 (動作確認)

```bash
make test
# または
ghc --make -isrc src/Main.hs -o cputest
./cputest
```

出力:
```
=== ComputerSystem Haskell Simulator ===
Total gates (sorted): 689
RegA = 42  (expected 42)
FlagZ = False
Halted = True
Done.
```

### cabal でビルド

```bash
cabal build
cabal run computersystem
```

### WASM ビルド (ブラウザ実行)

**Step 1: GHC WASM バックエンドをインストール**

```bash
curl https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta/raw/master/bootstrap.sh | sh
source ~/.ghc-wasm/env
```

**Step 2: WASM ファイルをビルド**

```bash
make wasm
# → web/computersystem.wasm が生成される
```

**Step 3: 開発サーバーを起動**

```bash
make serve
# → http://localhost:8080 を開く
```

---

## WASM インターフェース

Haskell 側でエクスポートする関数:

| 関数名 | シグネチャ | 説明 |
|--------|-----------|------|
| `hs_get_layout` | `() → CString` | CPU レイアウト JSON (初期化時に1度) |
| `hs_get_state`  | `() → CString` | 現在の CPU ステート JSON |
| `hs_get_wires`  | `() → CString` | 全ワイヤ値 JSON (レンダリング用) |
| `hs_step`       | `() → ()` | 1 命令実行 |
| `hs_reset`      | `() → ()` | CPU リセット |
| `hs_load_asm`   | `CString → ()` | アセンブリソースをロード |

JS からの呼び出し例:

```javascript
await HaskellCPU.init('computersystem.wasm');
const layout = HaskellCPU.getLayout();  // LayoutComp ツリー
renderer.setLayout(layout);

HaskellCPU.loadAsm('LOAD_A 25\nLOAD_B 17\nADD\nHLT');
HaskellCPU.step();
const state = HaskellCPU.getState();
// { regA: "0x2a", regAVal: 42, flagZ: false, halted: true, ... }
```

---

## アセンブリ例

```asm
; A + B = 42
LOAD_A 25
LOAD_B 17
ADD
HLT
```

```asm
; 10まで数える
LOAD_A 0
LOAD_B 1
ADD
STORE_A 0x80
LOAD_B 10
SUB
JZ 20         ; A == 0 なら終了へ
LOAD_A_MEM 0x80
LOAD_B 1
JMP 4         ; ループ先頭へ
HLT
```

---

## 回路構成

```
CPU (689 ゲート)
├── Register A, B (各 8bit × D-FF × 2 ラッチ)
├── IR_High (オペコード), IR_Low (オペランド)
├── Program Counter (レジスタ + MUX)
├── ALU
│   ├── B 反転 (XOR × 8, SUB 用)
│   ├── 8bit リップルキャリー加算器
│   │   └── FullAdder × 8
│   │       └── HalfAdder × 2 + OR × 1
│   ├── AND/OR/XOR 8bit ブロック
│   ├── 4-to-1 MUX (演算選択)
│   └── ゼロフラグ検出 (OR ツリー + NOT)
├── 命令デコーダ (AND × 13 + OR ツリー)
└── メモリ (256 バイト)
```

### D フリップフロップ (マスタースレーブ構成)

```
CLK ──→ [NOT] ──→ CLKbar
D ───→ [Master D-Latch (EN=CLK)] ──→ [Slave D-Latch (EN=CLKbar)] ──→ Q
```

各 D-Latch は SR ラッチ (NAND 2つのフィードバック) で実装。  
フィードバックループは `evaluate` の反復収束で解決。

---

## 元プロジェクト

[ComputerSystem](../ComputerSystem) — JavaScript 版 (同等機能、3000行 JS)

| | JavaScript 版 | Haskell 版 |
|--|--|--|
| シミュレーション | JS オブジェクト + 参照書き換え | 純粋関数 (WireVals) |
| 状態管理 | ミュータブル | イミュータブル |
| ゲート数 | 689 | 689 (同一) |
| ブラウザ実行 | ネイティブ JS | WASM |
| 依存パッケージ | なし | GHC 同梱のみ |
