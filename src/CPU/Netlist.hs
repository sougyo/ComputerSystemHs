{-# LANGUAGE StrictData #-}
-- CPU 回路の論理構造 (ネットリスト)
--
-- このモジュールはゲートの接続関係のみを定義する。
-- 座標・サイズ・配線の折れ曲がり位置などの視覚情報は CPU.Components を参照すること。
--
-- 読み方:
--   各関数の引数が「入力ワイヤ」、戻り値の *Net レコードが「出力ワイヤ群」を表す。
--   内部で addGate を呼ぶたびに論理ゲートが1つ追加される。
module CPU.Netlist
  ( -- SR ラッチ
    SRLatchNet(..), netSRLatch
    -- D ラッチ
  , DLatchNet(..), netDLatch
    -- D フリップフロップ
  , DFFNet(..), netDFF
    -- 8ビットレジスタ
  , netRegister8
    -- 加算器
  , HalfAdderNet(..), netHalfAdder
  , FullAdderNet(..), netFullAdder
  , netAdder8
    -- MUX
  , Mux2Net(..), netMux2
  , netMux4_8bit
    -- ビット演算
  , netBitwise8
  , netNot8
    -- 命令デコーダ
  , DecoderNet(..), netDecoder
    -- ALU
  , ALUNet(..), netALU
  ) where

import Control.Monad (foldM)
import Data.Bits (testBit)

import Circuit (GateType(..))
import Builder

-- ──────────────────────────────────────────────
-- SR ラッチ
-- ──────────────────────────────────────────────
-- NAND ゲート2つをクロス結合したフィードバック回路。
-- Sb=Low → Q=High にセット、Rb=Low → Q=Low にリセット。
-- Sb=High かつ Rb=High のとき直前の状態を保持。
data SRLatchNet = SRLatchNet { srnQ :: !Int, srnQb :: !Int }

netSRLatch :: Int -> Int -> Build SRLatchNet
netSRLatch wSb wRb = do
  wQ  <- freshWire
  wQb <- freshWire
  addGateFixed NAND [wSb, wQb] wQ    -- Q  = NAND(Sb, Qb)
  addGateFixed NAND [wRb,  wQ] wQb   -- Qb = NAND(Rb, Q)
  return SRLatchNet { srnQ = wQ, srnQb = wQb }

-- ──────────────────────────────────────────────
-- D ラッチ (透過型ラッチ)
-- ──────────────────────────────────────────────
-- EN=High の間は D の値がそのまま Q に伝わる (透過モード)。
-- EN=Low になると直前の値を保持する (ラッチモード)。
-- 内部は NAND ベースの SR ラッチで構成。
-- dlnDb/dlnSb/dlnRb はレイアウト層での LayoutGate 構築に使用する。
data DLatchNet = DLatchNet
  { dlnQ  :: !Int
  , dlnQb :: !Int
  , dlnDb :: !Int   -- NOT(D) の出力
  , dlnSb :: !Int   -- NAND(D,  EN) の出力 → SR ラッチの Sb 入力
  , dlnRb :: !Int   -- NAND(Db, EN) の出力 → SR ラッチの Rb 入力
  }

netDLatch :: Int -> Int -> Build DLatchNet
netDLatch wD wEN = do
  wDb <- addGate NOT  [wD]           -- D の反転
  wSb <- addGate NAND [wD,  wEN]     -- セット信号 (D=1 かつ EN=1 でアクティブ)
  wRb <- addGate NAND [wDb, wEN]     -- リセット信号 (D=0 かつ EN=1 でアクティブ)
  SRLatchNet wQ wQb <- netSRLatch wSb wRb
  return DLatchNet { dlnQ = wQ, dlnQb = wQb, dlnDb = wDb, dlnSb = wSb, dlnRb = wRb }

-- ──────────────────────────────────────────────
-- D フリップフロップ (マスタースレーブ型)
-- ──────────────────────────────────────────────
-- CLK の立ち上がりエッジで D の値をキャプチャし Q に出力する。
-- マスターラッチ (CLK=High で透過) とスレーブラッチ (CLK=Low で透過) の
-- 2段直列接続で、エッジトリガ動作を実現する。
-- dffnMaster/dffnSlave にはレイアウト構築に必要な中間ワイヤが含まれる。
data DFFNet = DFFNet
  { dffnQ      :: !Int
  , dffnQb     :: !Int
  , dffnClkb   :: !Int        -- NOT(CLK) の出力
  , dffnMaster :: !DLatchNet  -- マスターラッチの全ワイヤ (レイアウト用)
  , dffnSlave  :: !DLatchNet  -- スレーブラッチの全ワイヤ (レイアウト用)
  }

netDFF :: Int -> Int -> Build DFFNet
netDFF wD wCLK = do
  wClkb  <- addGate NOT [wCLK]
  master <- netDLatch wD          wCLK    -- マスター: CLK=High で D を取り込む
  slave  <- netDLatch (dlnQ master) wClkb -- スレーブ: CLK=Low  で出力を確定
  return DFFNet
    { dffnQ = dlnQ slave, dffnQb = dlnQb slave
    , dffnClkb   = wClkb
    , dffnMaster = master
    , dffnSlave  = slave
    }

-- ──────────────────────────────────────────────
-- 8ビットレジスタ
-- ──────────────────────────────────────────────
-- 8個の D フリップフロップを並べた汎用レジスタ。
-- 同じ CLK で全ビットを同時にキャプチャする。
netRegister8 :: [Int] -> Int -> Build [Int]
netRegister8 wDs wCLK = mapM (\wD -> dffnQ <$> netDFF wD wCLK) wDs

-- ──────────────────────────────────────────────
-- ハーフアダー
-- ──────────────────────────────────────────────
-- Sum   = A XOR B  (排他的論理和: 桁上がりを無視した和)
-- Carry = A AND B  (桁上がり: 両方 1 のときのみ発生)
data HalfAdderNet = HalfAdderNet { hanSum :: !Int, hanCarry :: !Int }

netHalfAdder :: Int -> Int -> Build HalfAdderNet
netHalfAdder wA wB = do
  wSum   <- addGate XOR [wA, wB]
  wCarry <- addGate AND [wA, wB]
  return HalfAdderNet { hanSum = wSum, hanCarry = wCarry }

-- ──────────────────────────────────────────────
-- フルアダー
-- ──────────────────────────────────────────────
-- Sum  = A XOR B XOR Cin   (3入力の排他的論理和)
-- Cout = 多数決(A, B, Cin)  (2つ以上が 1 なら桁上がり)
-- ハーフアダー2つと OR ゲート1つで構成。
-- 中間ワイヤはレイアウト層での LayoutComp 組み立てに使用する。
data FullAdderNet = FullAdderNet
  { fanSum  :: !Int   -- 最終和出力
  , fanCout :: !Int   -- 桁上がり出力
  , fanHA1Sum :: !Int -- 第1段ハーフアダーの和 (第2段への入力)
  , fanHA1C   :: !Int -- 第1段ハーフアダーのキャリー
  , fanHA2C   :: !Int -- 第2段ハーフアダーのキャリー
  }

netFullAdder :: Int -> Int -> Int -> Build FullAdderNet
netFullAdder wA wB wCin = do
  HalfAdderNet ha1Sum ha1Carry <- netHalfAdder wA     wB   -- 第1段: A+B
  HalfAdderNet wSum   ha2Carry <- netHalfAdder ha1Sum wCin -- 第2段: (A+B)+Cin
  wCout <- addGate OR [ha1Carry, ha2Carry]
  return FullAdderNet
    { fanSum = wSum, fanCout = wCout
    , fanHA1Sum = ha1Sum, fanHA1C = ha1Carry, fanHA2C = ha2Carry
    }

-- ──────────────────────────────────────────────
-- 8ビットリップルキャリー加算器
-- ──────────────────────────────────────────────
-- フルアダーを8個直列に繋ぎ、下位ビットの桁上がりを上位ビットに伝搬させる。
-- 構造が単純な反面、上位ビットの計算は下位ビットの桁上がりが確定するまで待つ必要がある。
netAdder8 :: [Int] -> [Int] -> Int -> Build ([Int], Int)
netAdder8 wAs wBs wCin = do
  (revSums, cout) <- foldM addBit ([], wCin) (zip wAs wBs)
  return (reverse revSums, cout)
  where
    addBit (sums, cin) (wA, wB) = do
      fa <- netFullAdder wA wB cin
      return (fanSum fa : sums, fanCout fa)

-- ──────────────────────────────────────────────
-- 1ビット 2-to-1 MUX
-- ──────────────────────────────────────────────
-- Sel=0 のとき A を、Sel=1 のとき B を出力する。
-- Out = (A AND NOT Sel) OR (B AND Sel)
-- 中間ワイヤも返すことで、レイアウト層が LayoutGate を構築できる。
data Mux2Net = Mux2Net
  { m2nOut    :: !Int   -- 選択結果出力
  , m2nNotSel :: !Int   -- NOT Sel
  , m2nPassA  :: !Int   -- A AND NOT Sel
  , m2nPassB  :: !Int   -- B AND Sel
  }

netMux2 :: Int -> Int -> Int -> Build Mux2Net
netMux2 wA wB wSel = do
  wNotSel <- addGate NOT [wSel]
  wPA     <- addGate AND [wA, wNotSel]
  wPB     <- addGate AND [wB, wSel]
  wOut    <- addGate OR  [wPA, wPB]
  return Mux2Net { m2nOut = wOut, m2nNotSel = wNotSel, m2nPassA = wPA, m2nPassB = wPB }

-- ──────────────────────────────────────────────
-- 8ビット 4-to-1 MUX
-- ──────────────────────────────────────────────
-- Sel1:Sel0 = 00→I0, 01→I1, 10→I2, 11→I3 を各ビット独立に選択。
-- 2-to-1 MUX を2段重ねて実現: 第1段で Sel0 により I0/I1 と I2/I3 を選択し、
-- 第2段で Sel1 により第1段の出力を選択する。
netMux4_8bit :: [Int] -> [Int] -> [Int] -> [Int] -> Int -> Int -> Build [Int]
netMux4_8bit wI0s wI1s wI2s wI3s wSel0 wSel1 =
  mapM selectBit (zip4n wI0s wI1s wI2s wI3s)
  where
    mux2out a b s = m2nOut <$> netMux2 a b s
    selectBit (w0, w1, w2, w3) = do
      wAB <- mux2out w0 w1 wSel0   -- I0/I1 から Sel0 で選択
      wCD <- mux2out w2 w3 wSel0   -- I2/I3 から Sel0 で選択
      mux2out wAB wCD wSel1         -- 上段結果から Sel1 で最終選択

zip4n :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4n (a:as) (b:bs) (c:cs) (d:ds) = (a, b, c, d) : zip4n as bs cs ds
zip4n _ _ _ _ = []

-- ──────────────────────────────────────────────
-- 8ビット ビット演算
-- ──────────────────────────────────────────────
-- 同じ型のゲートを8ビット分並べた演算ブロック。
-- AND/OR/XOR などを gtype で指定する。
netBitwise8 :: GateType -> [Int] -> [Int] -> Build [Int]
netBitwise8 gtype wAs wBs = mapM (\(a, b) -> addGate gtype [a, b]) (zip wAs wBs)

-- ──────────────────────────────────────────────
-- 8ビット NOT
-- ──────────────────────────────────────────────
netNot8 :: [Int] -> Build [Int]
netNot8 = mapM (\wA -> addGate NOT [wA])

-- ──────────────────────────────────────────────
-- 命令デコーダ
-- ──────────────────────────────────────────────
-- PLA (Programmable Logic Array) 構造:
--
--   AND 平面: 4ビットオペコードの各パターンを検出するANDゲート群
--             各ビットの正転 or 反転を組み合わせて1命令を特定する
--
--   OR 平面: 複数の命令が共通して必要とする制御信号を生成するORゲート群
--            例) RegWrite は LoadA/LoadB/LoadAM/Add/Sub/And/Or/Xor/Not の
--                どれかが有効なら High になる
--
-- opBits の下位4ビット (bits 0-3) のみをデコードに使用する。
data DecoderNet = DecoderNet
  { -- layout ルーティング用の中間ワイヤ (CPU.Components で使用)
    dnnOpInvs   :: ![Int]   -- NOT(opBits[0..3])
  , dnnDetWires :: ![Int]   -- AND ゲート出力 (命令検出, 順序は decPats と対応)
    -- 制御信号出力
  , dnnRegWrite :: !Int     -- レジスタ書き込み許可
  , dnnMemRead  :: !Int     -- メモリ読み出し許可
  , dnnMemWrite :: !Int     -- メモリ書き込み許可
  , dnnAluOp0   :: !Int     -- ALU 演算選択 ビット0
  , dnnAluOp1   :: !Int     -- ALU 演算選択 ビット1
  , dnnSubMode  :: !Int     -- 減算モード (加算器への Cin と B反転を兼ねる)
  , dnnBranch   :: !Int     -- 無条件ジャンプ
  , dnnBranchZ  :: !Int     -- ゼロ条件ジャンプ
  , dnnHalt     :: !Int     -- 停止
  , dnnLoadImm  :: !Int     -- 即値ロード (オペランドをデータバスに乗せる)
  , dnnRegDstB  :: !Int     -- 書き込み先レジスタを B に切り替え
  }

-- オペコードパターン一覧 (AND 平面の各行に対応)
decPats :: [Int]
decPats = [0x1,0x2,0x3,0x4,0x5,0x6,0x7,0x8,0x9,0xA,0xB,0xC,0xF]

netDecoder :: [Int] -> Build DecoderNet
netDecoder opBits = do
  -- AND 平面前段: 各入力ビットの反転を生成
  opInvs <- mapM (\w -> addGate NOT [w]) (take 4 opBits)

  -- AND 平面: パターンが一致したビットを選んで AND を取り命令を検出
  let sel b pat = if testBit pat b then opBits !! b else opInvs !! b
      detect pat = addGate AND [sel 0 pat, sel 1 pat, sel 2 pat, sel 3 pat]
  detWires <- mapM detect decPats
  let isLoadA  = detWires !! 0;  isLoadB  = detWires !! 1;  isLoadAM = detWires !! 2
      isStoreA = detWires !! 3;  isAdd    = detWires !! 4;  isSub    = detWires !! 5
      isAnd    = detWires !! 6;  isOr     = detWires !! 7;  isXor    = detWires !! 8
      isNot    = detWires !! 9;  isJmp    = detWires !! 10; isJz     = detWires !! 11
      isHlt    = detWires !! 12

  -- OR 平面: 各制御信号を生成
  let ctrl ws = case ws of
        [w] -> return w          -- 1命令だけなら OR 不要
        _   -> addGate OR ws
  wRegWrite <- ctrl [isLoadA, isLoadB, isLoadAM, isAdd, isSub, isAnd, isOr, isXor, isNot]
  wMemRead  <- ctrl [isLoadAM]
  wMemWrite <- ctrl [isStoreA]
  wAluOp0   <- ctrl [isAnd, isXor]
  wAluOp1   <- ctrl [isOr,  isXor]
  wSubMode  <- ctrl [isSub]
  wBranch   <- ctrl [isJmp]
  wBranchZ  <- ctrl [isJz]
  wHalt     <- ctrl [isHlt]
  wLoadImm  <- ctrl [isLoadA, isLoadB]
  wRegDstB  <- ctrl [isLoadB]

  return DecoderNet
    { dnnOpInvs   = opInvs
    , dnnDetWires = detWires
    , dnnRegWrite = wRegWrite, dnnMemRead  = wMemRead, dnnMemWrite = wMemWrite
    , dnnAluOp0   = wAluOp0,  dnnAluOp1   = wAluOp1,  dnnSubMode  = wSubMode
    , dnnBranch   = wBranch,  dnnBranchZ  = wBranchZ,  dnnHalt     = wHalt
    , dnnLoadImm  = wLoadImm, dnnRegDstB  = wRegDstB
    }

-- ──────────────────────────────────────────────
-- ALU (8ビット 4演算)
-- ──────────────────────────────────────────────
-- Sel1:Sel0 で演算を選択する:
--   00 = 加算 (または SubMode=1 で減算)
--   01 = AND
--   10 = OR
--   11 = XOR
--
-- 減算は B の各ビットを SubMode で XOR して反転し、
-- 加算器の Cin に SubMode を入力することで 2の補数加算として実現する。
-- (B XOR 1 で反転、Cin=1 で +1 → NOT(B)+1 = -B)
--
-- ゼロフラグは演算結果8ビットの OR ツリーを NOT して生成する。
data ALUNet = ALUNet
  { alnResults :: ![Int]   -- 演算結果 8ビット (LSB から順)
  , alnZero    :: !Int     -- ゼロフラグ (結果が全ビット 0 のとき High)
  , alnCout    :: !Int     -- 加算器のキャリー出力
  }

netALU :: [Int] -> [Int] -> Int -> Int -> Int -> Build ALUNet
netALU wAs wBs wSel0 wSel1 wSubMode = do
  -- 減算モード: B を SubMode で XOR して必要に応じて反転
  wBinvs       <- netBitwise8 XOR wBs (replicate 8 wSubMode)
  -- 加算 (Cin = SubMode: 減算時は +1 して2の補数を完成させる)
  (sums, cout) <- netAdder8 wAs wBinvs wSubMode
  -- ビット演算ブロック (加算器とは独立して並列計算)
  andOuts      <- netBitwise8 AND wAs wBs
  orOuts       <- netBitwise8 OR  wAs wBs
  xorOuts      <- netBitwise8 XOR wAs wBs
  -- MUX で演算結果を選択
  muxOuts      <- netMux4_8bit sums andOuts orOuts xorOuts wSel0 wSel1
  -- ゼロフラグ: 8ビット OR ツリー → NOT
  wZ1 <- addGate OR [muxOuts!!0, muxOuts!!1]
  wZ2 <- addGate OR [muxOuts!!2, muxOuts!!3]
  wZ3 <- addGate OR [muxOuts!!4, muxOuts!!5]
  wZ4 <- addGate OR [muxOuts!!6, muxOuts!!7]
  wZ5 <- addGate OR [wZ1, wZ2]
  wZ6 <- addGate OR [wZ3, wZ4]
  wZ7 <- addGate OR [wZ5, wZ6]
  wZero <- addGate NOT [wZ7]
  return ALUNet { alnResults = muxOuts, alnZero = wZero, alnCout = cout }
