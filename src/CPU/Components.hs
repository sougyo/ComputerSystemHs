{-# LANGUAGE StrictData #-}
module CPU.Components
  ( LayoutComp(..)
  , LayoutGate(..)
  , LayoutPin(..)
  , LayoutSeg(..)
  , buildCPU
  , CPUBuildResult(..)
  ) where

import Control.Monad (forM, foldM)
import Control.Monad.State.Strict (get)

import Circuit (GateType(..), Gate(..))
import Builder
import CPU.Types
import CPU.Netlist
  ( SRLatchNet(..), netSRLatch
  , DLatchNet(..), netDLatch
  , DFFNet(..), netDFF
  , HalfAdderNet(..), netHalfAdder
  , FullAdderNet(..), netFullAdder
  , netBitwise8, netNot8
  , Mux2Net(..), netMux2
  )

-- ──────────────────────────────────────────────
-- レイアウト型
-- ──────────────────────────────────────────────

data LayoutSeg = LayoutSeg
  { lsX1, lsY1, lsX2, lsY2 :: !Double
  , lsOwner :: !Int
  } deriving Show

data LayoutPin = LayoutPin
  { lpWire :: !Int
  , lpX, lpY :: !Double
  } deriving Show

data LayoutGate = LayoutGate
  { lgId   :: !Int
  , lgType :: !String
  , lgName :: !String
  , lgX, lgY, lgW, lgH :: !Double
  , lgIns  :: ![Int]
  , lgOut  :: !Int
  } deriving Show

data LayoutComp = LayoutComp
  { lcId       :: !Int
  , lcType     :: !String
  , lcName     :: !String
  , lcLabel    :: !String
  , lcX, lcY  :: !Double
  , lcW, lcH  :: !Double
  , lcColor    :: !String
  , lcChildren :: ![LayoutComp]
  , lcGates    :: ![LayoutGate]
  , lcWireSegs :: ![(Int, [LayoutSeg])]
  , lcInputs   :: ![(String, LayoutPin)]
  , lcOutputs  :: ![(String, LayoutPin)]
  , lcExpandAt :: !Double
  } deriving Show

-- ゲートを追加し LayoutGate と出力ワイヤIDを返す
-- ゲートID (lgId) には出力ワイヤIDを使用 (ワイヤIDは全体でユニーク)
lgate :: String -> String -> Double -> Double -> Double -> Double -> [Int]
      -> Build (LayoutGate, Int)
lgate typ name x y w h ins = do
  out <- addGate (readGType typ) ins
  return (LayoutGate out typ name x y w h ins out, out)

readGType :: String -> GateType
readGType "AND"  = AND
readGType "OR"   = OR
readGType "NAND" = NAND
readGType "NOR"  = NOR
readGType "XOR"  = XOR
readGType "XNOR" = XNOR
readGType "NOT"  = NOT
readGType _      = BUF

-- Int → Double ヘルパー
d :: Int -> Double
d = fromIntegral

-- ──────────────────────────────────────────────
-- SR ラッチ (NAND 2つのクロス結合)
-- ──────────────────────────────────────────────
buildSRLatch :: String -> Double -> Double -> Int -> Int
             -> Build (LayoutComp, Int, Int)
buildSRLatch name x y wSb wRb = do
  net <- netSRLatch wSb wRb
  comp <- layoutSRLatch name x y wSb wRb net
  return (comp, srnQ net, srnQb net)

layoutSRLatch :: String -> Double -> Double -> Int -> Int -> SRLatchNet
              -> Build LayoutComp
layoutSRLatch name x y wSb wRb (SRLatchNet wQ wQb) = do
  cid <- freshComp
  let lg1 = LayoutGate wQ  "NAND" (name++"_n1") 6 1 5 4 [wSb,wQb] wQ
      lg2 = LayoutGate wQb "NAND" (name++"_n2") 6 8 5 4 [wRb,wQ]  wQb
      segs = [ (wSb,  [LayoutSeg 0 3 6 3 cid])
             , (wRb,  [LayoutSeg 0 11 6 10 cid])
             , (wQ,   [LayoutSeg 11 3 20 3 cid
                      ,LayoutSeg 13 3 13 10 cid, LayoutSeg 13 10 6 12 cid])
             , (wQb,  [LayoutSeg 11 10 20 11 cid
                      ,LayoutSeg 13 10 13 3 cid, LayoutSeg 13 3 6 5 cid])
             ]
  return $ LayoutComp cid "SR_LATCH" name name x y 20 14 "#00cec9"
             [] [lg1,lg2] segs
             [("Sb", LayoutPin wSb 0 3), ("Rb", LayoutPin wRb 0 11)]
             [("Q",  LayoutPin wQ  20 3), ("Qb", LayoutPin wQb 20 11)]
             0

-- ──────────────────────────────────────────────
-- D ラッチ (EN=1で透過)
-- ──────────────────────────────────────────────
buildDLatch :: String -> Double -> Double -> Int -> Int
            -> Build (LayoutComp, Int, Int)
buildDLatch name x y wD wEN = do
  net <- netDLatch wD wEN
  comp <- layoutDLatch name x y wD wEN net
  return (comp, dlnQ net, dlnQb net)

layoutDLatch :: String -> Double -> Double -> Int -> Int -> DLatchNet
             -> Build LayoutComp
layoutDLatch name x y wD wEN dln = do
  cid <- freshComp
  let wDb = dlnDb dln; wSb = dlnSb dln; wRb = dlnRb dln
      wQ  = dlnQ  dln; wQb = dlnQb dln
      lgNotD = LayoutGate wDb "NOT"  (name++"_notD") 3  2  4 3 [wD]        wDb
      lgN1   = LayoutGate wSb "NAND" (name++"_g1")   12 1  5 4 [wD,  wEN]  wSb
      lgN2   = LayoutGate wRb "NAND" (name++"_g2")   12 14 5 4 [wDb, wEN]  wRb
  sr <- layoutSRLatch (name++"_sr") 22 3 wSb wRb (SRLatchNet wQ wQb)
  let segs = [ (wD,  [LayoutSeg 0 4 10 4 cid, LayoutSeg 3 4 3 3.5 cid
                     ,LayoutSeg 10 4 10 2.33 cid, LayoutSeg 10 2.33 12 2.33 cid])
             , (wEN, [LayoutSeg 0 18 11 18 cid, LayoutSeg 11 3.67 11 18 cid
                     ,LayoutSeg 11 3.67 12 3.67 cid, LayoutSeg 11 16.67 12 16.67 cid])
             , (wDb, [LayoutSeg 7 3.5 9 3.5 cid, LayoutSeg 9 3.5 9 15.33 cid
                     ,LayoutSeg 9 15.33 12 15.33 cid])
             , (wSb, [LayoutSeg 17 3 20 3 cid, LayoutSeg 20 3 20 6 cid
                     ,LayoutSeg 20 6 22 6 cid])
             , (wRb, [LayoutSeg 17 16 20 16 cid, LayoutSeg 20 16 20 14 cid
                     ,LayoutSeg 20 14 22 14 cid])
             ]
  return $ LayoutComp cid "D_LATCH" name name x y 44 22 "#00b894"
             [sr] [lgNotD, lgN1, lgN2] segs
             [("D",  LayoutPin wD 0 4), ("EN", LayoutPin wEN 0 18)]
             [("Q",  LayoutPin wQ 44 6), ("Qb", LayoutPin wQb 44 16)]
             0

-- ──────────────────────────────────────────────
-- D フリップフロップ (マスタースレーブ)
-- ──────────────────────────────────────────────
buildDFF :: String -> Double -> Double -> Int -> Int
         -> Build (LayoutComp, Int, Int)
buildDFF name x y wD wCLK = do
  net <- netDFF wD wCLK
  comp <- layoutDFF name x y wD wCLK net
  return (comp, dffnQ net, dffnQb net)

layoutDFF :: String -> Double -> Double -> Int -> Int -> DFFNet
          -> Build LayoutComp
layoutDFF name x y wD wCLK dfn = do
  cid <- freshComp
  let wClkBar = dffnClkb dfn
      wMQ     = dlnQ (dffnMaster dfn)
      wQ      = dffnQ dfn
      wQb     = dffnQb dfn
      lgNotClk = LayoutGate wClkBar "NOT" (name++"_notclk") 49 12 4 4 [wCLK] wClkBar
  master <- layoutDLatch (name++"_master") 5  3 wD  wCLK    (dffnMaster dfn)
  slave  <- layoutDLatch (name++"_slave")  53 3 wMQ wClkBar (dffnSlave  dfn)
  let segs = [ (wD,      [LayoutSeg 0 4 5 4 cid, LayoutSeg 5 4 5 7 cid])
             , (wCLK,    [LayoutSeg 0 22 3 22 cid, LayoutSeg 3 22 3 14 cid
                         ,LayoutSeg 3 21 5 21 cid, LayoutSeg 3 14 49 14 cid])
             , (wClkBar, [LayoutSeg 53 14 55 14 cid, LayoutSeg 55 14 55 21 cid
                         ,LayoutSeg 55 21 53 21 cid])
             , (wMQ,     [LayoutSeg 49 9 51 9 cid, LayoutSeg 51 9 51 7 cid
                         ,LayoutSeg 51 7 53 7 cid])
             , (wQ,      [LayoutSeg 97 9 99 9 cid, LayoutSeg 99 9 99 6 cid
                         ,LayoutSeg 99 6 100 6 cid])
             , (wQb,     [LayoutSeg 97 19 100 19 cid])
             ]
  return $ LayoutComp cid "DFF" name name x y 100 28 "#00cec9"
             [master, slave] [lgNotClk] segs
             [("D", LayoutPin wD 0 4), ("CLK", LayoutPin wCLK 0 22)]
             [("Q", LayoutPin wQ 100 6), ("Qb", LayoutPin wQb 100 19)]
             120

-- ──────────────────────────────────────────────
-- 8ビットレジスタ (8個のDFF)
-- ──────────────────────────────────────────────
buildRegister8 :: String -> Double -> Double -> [Int] -> Int
               -> Build (LayoutComp, [Int])
buildRegister8 name x y wDs wCLK = do
  cid <- freshComp
  results <- forM (zip wDs [0..7]) $ \(wD, i) -> do
    (dff, wQ, _) <- buildDFF (name++"_DFF"++show (i::Int)) 8 (5 + d i*28) wD wCLK
    return (dff, wQ)
  let (dffs, wQs) = unzip results
      ins  = zipWith (\wD i -> ("D"++show (i::Int), LayoutPin wD 0 (10+d i*28))) wDs [0..7]
      outs = zipWith (\wQ i -> ("Q"++show (i::Int), LayoutPin wQ 116 (10+d i*28))) wQs [0..7]
  let comp = LayoutComp cid "REGISTER" name name x y 116 240 "#0984e3"
               dffs [] []
               (("CLK", LayoutPin wCLK 0 235) : ins) outs
               0
  return (comp, wQs)

-- ──────────────────────────────────────────────
-- ハーフアダー: XOR + AND
-- ──────────────────────────────────────────────
buildHalfAdder :: String -> Double -> Double -> Int -> Int
               -> Build (LayoutComp, Int, Int)
buildHalfAdder name x y wA wB = do
  net <- netHalfAdder wA wB
  comp <- layoutHalfAdder name x y wA wB net
  return (comp, hanSum net, hanCarry net)

-- ハーフアダーのレイアウトのみ組み立てる (ゲートは net で登録済み)
layoutHalfAdder :: String -> Double -> Double -> Int -> Int -> HalfAdderNet
                -> Build LayoutComp
layoutHalfAdder name x y wA wB (HalfAdderNet wSum wCarry) = do
  cid <- freshComp
  let lgXor = LayoutGate wSum   "XOR" (name++"_xor") 10 1 6 4 [wA, wB] wSum
      lgAnd = LayoutGate wCarry "AND" (name++"_and") 10 9 6 4 [wA, wB] wCarry
      segs  = [ (wA,     [ LayoutSeg 0 4  2  4 cid
                         , LayoutSeg 2 4  2  3 cid, LayoutSeg 2  3 10  3 cid
                         , LayoutSeg 2 4  2 11 cid, LayoutSeg 2 11 10 11 cid ])
              , (wB,     [ LayoutSeg 0 12 6 12 cid
                         , LayoutSeg 6 12 6  5 cid, LayoutSeg 6  5 10  5 cid
                         , LayoutSeg 6 12 6 13 cid, LayoutSeg 6 13 10 13 cid ])
              , (wSum,   [LayoutSeg 16 3 24 4 cid])
              , (wCarry, [LayoutSeg 16 11 24 12 cid])
              ]
  return $ LayoutComp cid "HALF_ADDER" name name x y 24 16 "#6c5ce7"
             [] [lgXor, lgAnd] segs
             [("A", LayoutPin wA 0 4), ("B", LayoutPin wB 0 12)]
             [("Sum", LayoutPin wSum 24 4), ("Carry", LayoutPin wCarry 24 12)]
             0

-- ──────────────────────────────────────────────
-- フルアダー: HA×2 + OR
-- ──────────────────────────────────────────────
buildFullAdder :: String -> Double -> Double -> Int -> Int -> Int
               -> Build (LayoutComp, Int, Int)
buildFullAdder name x y wA wB wCin = do
  cid <- freshComp
  FullAdderNet wSum wCout ha1Sum ha1Carry ha2Carry <- netFullAdder wA wB wCin
  ha1 <- layoutHalfAdder (name++"_HA1") 5  2  wA     wB   (HalfAdderNet ha1Sum ha1Carry)
  ha2 <- layoutHalfAdder (name++"_HA2") 5  20 ha1Sum wCin (HalfAdderNet wSum   ha2Carry)
  let lgOr = LayoutGate wCout "OR" (name++"_or") 38 16 5 3 [ha1Carry, ha2Carry] wCout
  let segs = [ (wA,       [LayoutSeg 0 8 5 6 cid])
             , (wB,       [LayoutSeg 0 16 5 14 cid])
             , (wCin,     [LayoutSeg 0 32 5 32 cid])
             , (ha1Sum,   [ LayoutSeg 29  6 30  6 cid
                          , LayoutSeg 30  6 30 19 cid
                          , LayoutSeg 30 19  3 19 cid
                          , LayoutSeg  3 19  3 24 cid
                          , LayoutSeg  3 24  5 24 cid ])
             , (ha1Carry, [ LayoutSeg 29 14 38 14 cid
                          , LayoutSeg 38 14 38 17 cid ])
             , (ha2Carry, [ LayoutSeg 29 32 38 32 cid
                          , LayoutSeg 38 32 38 19 cid ])
             , (wCout,    [LayoutSeg 43 18 55 18 cid])
             , (wSum,     [ LayoutSeg 29 24 36 24 cid
                          , LayoutSeg 36 24 36  8 cid
                          , LayoutSeg 36  8 55  8 cid ])
             ]
  let comp = LayoutComp cid "FULL_ADDER" name name x y 55 40 "#a29bfe"
               [ha1, ha2] [lgOr] segs
               [("A",LayoutPin wA 0 8),("B",LayoutPin wB 0 16),("Cin",LayoutPin wCin 0 32)]
               [("Sum",LayoutPin wSum 55 8),("Cout",LayoutPin wCout 55 18)]
               0
  return (comp, wSum, wCout)

-- ──────────────────────────────────────────────
-- 8ビットリップルキャリー加算器
-- ──────────────────────────────────────────────
buildAdder8 :: String -> Double -> Double -> [Int] -> [Int] -> Int
            -> Build (LayoutComp, [Int], Int)
buildAdder8 name x y wAs wBs wCin = do
  cid <- freshComp
  (fas, sums, lastCout) <- foldM step ([], [], wCin) (zip3 wAs wBs [0..7])
  let mkSeg (wA, wB, i) =
        [(wA, [LayoutSeg (5+d i*58+2) 0 (5+d i*58) 13 cid])
        ,(wB, [LayoutSeg (5+d i*58+12) 0 (5+d i*58) 21 cid])]
      segs = concatMap mkSeg (zip3 wAs wBs [0..7])
      ins  = ("Cin", LayoutPin wCin 0 45)
           : concatMap (\(wA,wB,i) ->
               [("A"++show (i::Int), LayoutPin wA (5+d i*58+2) 0)
               ,("B"++show (i::Int), LayoutPin wB (5+d i*58+12) 0)])
               (zip3 wAs wBs [0..7])
      outs = ("Cout", LayoutPin lastCout 480 45)
           : zipWith (\s i -> ("S"++show (i::Int), LayoutPin s (5+d i*58+55) 13)) sums [0..7]
  let comp = LayoutComp cid "ADDER_8BIT" name name x y 480 55 "#0984e3"
               fas [] segs ins outs 0
  return (comp, sums, lastCout)
  where
    step (faDone, sumsDone, cin) (wA, wB, i) = do
      (fa, faSum, faCout) <- buildFullAdder
        (name++"_FA"++show (i::Int)) (5+d i*58) 5 wA wB cin
      return (faDone++[fa], sumsDone++[faSum], faCout)

-- ──────────────────────────────────────────────
-- 8ビット ビット演算ブロック
-- ──────────────────────────────────────────────
buildBitwise8 :: String -> String -> String -> Double -> Double -> [Int] -> [Int]
              -> Build (LayoutComp, [Int])
buildBitwise8 name typ color x y wAs wBs = do
  cid <- freshComp
  wRs <- netBitwise8 (readGType typ) wAs wBs
  let lgs = [ LayoutGate wR typ (name++"_"++typ++show (i::Int))
                (5+d i*58) 5 6 3 [wA, wB] wR
            | (wR, wA, wB, i) <- zip4 wRs wAs wBs [0..7] ]
      segs = concatMap (\(wA,wB,wR,i) ->
               [(wA, [LayoutSeg (5+d i*58) 0 (5+d i*58) 7 cid])
               ,(wB, [LayoutSeg (5+d i*58+3) 0 (5+d i*58+3) 9 cid])
               ,(wR, [LayoutSeg (11+d i*58) 7 (11+d i*58) 20 cid])
               ]) (zip4 wAs wBs wRs [0..7])
      ins  = concatMap (\(wA,wB,i) ->
               [("A"++show (i::Int), LayoutPin wA (5+d i*58) 0)
               ,("B"++show (i::Int), LayoutPin wB (5+d i*58+3) 0)])
               (zip3 wAs wBs [0..7])
      outs = zipWith (\wR i -> ("R"++show (i::Int), LayoutPin wR (11+d i*58) 7)) wRs [0..7]
  let comp = LayoutComp cid ("BITWISE_"++typ) name name x y 480 20 color
               [] lgs segs ins outs 0
  return (comp, wRs)

zip4 :: [a]->[b]->[c]->[d]->[(a,b,c,d)]
zip4 (a:as)(b:bs)(c:cs)(dd:ds) = (a,b,c,dd):zip4 as bs cs ds
zip4 _ _ _ _ = []

-- ──────────────────────────────────────────────
-- 8ビット NOT ブロック
-- ──────────────────────────────────────────────
buildNot8 :: String -> Double -> Double -> [Int] -> Build (LayoutComp, [Int])
buildNot8 name x y wAs = do
  cid <- freshComp
  wRs <- netNot8 wAs
  let lgs = [ LayoutGate wR "NOT" (name++"_NOT"++show (i::Int))
                (5+d i*58) 5 6 3 [wA] wR
            | (wR, wA, i) <- zip3 wRs wAs [0..7] ]
      ins  = zipWith (\wA i -> ("A"++show (i::Int), LayoutPin wA (5+d i*58) 0)) wAs [0..7]
      outs = zipWith (\wR i -> ("R"++show (i::Int), LayoutPin wR (11+d i*58) 7)) wRs [0..7]
  let comp = LayoutComp cid "NOT_8BIT" name name x y 480 18 "#d63031"
               [] lgs [] ins outs 0
  return (comp, wRs)

-- ──────────────────────────────────────────────
-- 2-to-1 MUX (1ビット): NOT + AND×2 + OR
-- ──────────────────────────────────────────────
buildMux2 :: String -> Double -> Double -> Int -> Int -> Int
          -> Build (LayoutComp, Int)
buildMux2 name x y wA wB wSel = do
  cid <- freshComp
  Mux2Net wOut wNotSel wA1 wA2 <- netMux2 wA wB wSel
  -- ゲートをy=4以降に配置してラベル文字との重なりを回避 (高さ18→22)
  let lgNot  = LayoutGate wNotSel "NOT" (name++"_not")  3  5 4 3 [wSel]        wNotSel
      lgAnd1 = LayoutGate wA1     "AND" (name++"_and1") 10 4 6 3 [wA, wNotSel] wA1
      lgAnd2 = LayoutGate wA2     "AND" (name++"_and2") 10 12 6 3 [wB, wSel]   wA2
      lgOr   = LayoutGate wOut    "OR"  (name++"_or")   18 8 6 3 [wA1, wA2]    wOut
      comp = LayoutComp cid "MUX2" name name x y 24 22 "#e84393"
               [] [lgNot, lgAnd1, lgAnd2, lgOr] []
               [("A",LayoutPin wA 0 6),("B",LayoutPin wB 0 14),("Sel",LayoutPin wSel 0 20)]
               [("Out", LayoutPin wOut 24 10)]
               0
  return (comp, wOut)

-- ──────────────────────────────────────────────
-- 8ビット 4-to-1 MUX
-- ──────────────────────────────────────────────
buildMux4_8bit :: String -> Double -> Double
               -> [Int] -> [Int] -> [Int] -> [Int] -> Int -> Int
               -> Build (LayoutComp, [Int])
buildMux4_8bit name x y wI0s wI1s wI2s wI3s wSel0 wSel1 = do
  cid <- freshComp
  -- ビット間隔27 (MUX2幅24+余白3) で重ならないように配置
  results <- forM (zip5 wI0s wI1s wI2s wI3s [0..7]) $ \(w0,w1,w2,w3,i) -> do
    (ma, outA) <- buildMux2 (name++"_m1a_"++show (i::Int)) (10+d i*27) 5  w0 w1 wSel0
    (mb, outB) <- buildMux2 (name++"_m1b_"++show (i::Int)) (10+d i*27) 30 w2 w3 wSel0
    (mc, outC) <- buildMux2 (name++"_m2_" ++show (i::Int)) (10+d i*27) 58 outA outB wSel1
    -- 段間配線: m1a/m1b出力(右辺) → m2入力(左辺) を右→下→左でルーティング
    let xo = 10 + d i * 27   -- 各ビット列左端
        xr = xo + 24          -- MUX2右端 (= 34 + i*27)
        xg = xr + 1           -- 列間ギャップ内 (= 35 + i*27)
        xl = xo - 2           -- m2左端の2単位手前 (= 8 + i*27)
    -- MUX2 h=22: m1a出力y=5+10=15, m1b出力y=30+10=40, m2入力A y=58+6=64, 入力B y=58+14=72
    let segA = (outA, [ LayoutSeg xr 15 xg 15 cid   -- m1a出力 → ギャップへ
                      , LayoutSeg xg 15 xg 54 cid   -- 下へ (m1b底52-m2天58のギャップ)
                      , LayoutSeg xg 54 xl 54 cid   -- 左へ
                      , LayoutSeg xl 54 xl 64 cid   -- 下へ m2入力A(y=64)へ
                      , LayoutSeg xl 64 xo 64 cid ])-- m2入力Aへ
        segB = (outB, [ LayoutSeg xr 40 xg 40 cid   -- m1b出力 → ギャップへ
                      , LayoutSeg xg 40 xg 56 cid   -- 下へ
                      , LayoutSeg xg 56 xl 56 cid   -- 左へ
                      , LayoutSeg xl 56 xl 72 cid   -- 下へ m2入力B(y=72)へ
                      , LayoutSeg xl 72 xo 72 cid ])-- m2入力Bへ
    return ([ma,mb,mc], outC, [segA, segB])
  let (childGroups, outs, segGroups) = unzip3 results
      allChildren = concat childGroups
      allSegs     = concat segGroups
      selIns = [("Sel0",LayoutPin wSel0 105 90),("Sel1",LayoutPin wSel1 125 90)]
      bitIns = concatMap (\(w0,w1,w2,w3,i) ->
                 [("I0_"++show (i::Int),LayoutPin w0 0 8)
                 ,("I1_"++show (i::Int),LayoutPin w1 0 18)
                 ,("I2_"++show (i::Int),LayoutPin w2 0 38)
                 ,("I3_"++show (i::Int),LayoutPin w3 0 48)])
                 (zip5 wI0s wI1s wI2s wI3s [0..7])
      -- 最終ビット右端: 10+7*27+24 = 233; 出力ピンy = m2 y(58) + Out pin y(10) = 68
      bitOuts = zipWith (\o i -> ("O"++show (i::Int), LayoutPin o 237 68)) outs [0..7]
  let comp = LayoutComp cid "MUX4_8BIT" name name x y 240 90 "#e84393"
               allChildren [] allSegs (selIns++bitIns) bitOuts 0
  return (comp, outs)

zip5 :: [a]->[b]->[c]->[d]->[e]->[(a,b,c,d,e)]
zip5 (a:as)(b:bs)(c:cs)(dd:ds)(e:es) = (a,b,c,dd,e):zip5 as bs cs ds es
zip5 _ _ _ _ _ = []

-- ──────────────────────────────────────────────
-- 8ビット ALU
-- ──────────────────────────────────────────────
buildALU :: String -> Double -> Double -> [Int] -> [Int]
         -> Int -> Int -> Int
         -> Build (LayoutComp, [Int], Int, Int)
buildALU name x y wAs wBs wSel0 wSel1 wSubMode = do
  cid <- freshComp
  (bInvComp, wBinvs) <- buildBitwise8 (name++"_binv") "XOR" "#636e72" 10 5
                          wBs (replicate 8 wSubMode)
  (adder, sums, cout) <- buildAdder8 (name++"_add") 10 30 wAs wBinvs wSubMode
  (andBlk, andOuts)   <- buildBitwise8 (name++"_and") "AND" "#00b894" 10 95  wAs wBs
  (orBlk,  orOuts)    <- buildBitwise8 (name++"_or")  "OR"  "#fdcb6e" 10 125 wAs wBs
  (xorBlk, xorOuts)   <- buildBitwise8 (name++"_xor") "XOR" "#e17055" 10 155 wAs wBs
  (mux, muxOuts) <- buildMux4_8bit (name++"_mux") 10 180
                      sums andOuts orOuts xorOuts wSel0 wSel1
  -- ゼロフラグ検出 OR ツリー (MUXの右側 x=255〜 に配置して ALU 枠内に収める)
  (_, wz1) <- lgate "OR" (name++"_zor1") 255 183 5 3 [muxOuts!!0, muxOuts!!1]
  (_, wz2) <- lgate "OR" (name++"_zor2") 255 196 5 3 [muxOuts!!2, muxOuts!!3]
  (_, wz3) <- lgate "OR" (name++"_zor3") 255 209 5 3 [muxOuts!!4, muxOuts!!5]
  (_, wz4) <- lgate "OR" (name++"_zor4") 255 222 5 3 [muxOuts!!6, muxOuts!!7]
  (_, wz5) <- lgate "OR" (name++"_zor5") 270 189 5 3 [wz1, wz2]
  (_, wz6) <- lgate "OR" (name++"_zor6") 270 215 5 3 [wz3, wz4]
  (_, wz7) <- lgate "OR" (name++"_zor7") 285 202 5 3 [wz5, wz6]
  (lgNotZ, wZero) <- lgate "NOT" (name++"_notZ") 300 202 4 3 [wz7]
  let ins  = [("Op0",LayoutPin wSel0 200 280),("Op1",LayoutPin wSel1 220 280)
             ,("Sub",LayoutPin wSubMode 240 280)]
             ++ zipWith (\w i -> ("A"++show (i::Int), LayoutPin w 0 (40+d i*6))) wAs [0..7]
             ++ zipWith (\w i -> ("B"++show (i::Int), LayoutPin w 0 (100+d i*6))) wBs [0..7]
      outs = [("Zero",LayoutPin wZero 520 250),("Cout",LayoutPin cout 520 260)]
             ++ zipWith (\w i -> ("R"++show (i::Int), LayoutPin w 520 (190+d i*6))) muxOuts [0..7]
  let comp = LayoutComp cid "ALU" name "ALU" x y 520 280 "#e17055"
               [bInvComp, adder, andBlk, orBlk, xorBlk, mux]
               [lgNotZ] []
               ins outs 0
  return (comp, muxOuts, wZero, cout)

-- ──────────────────────────────────────────────
-- 命令デコーダ
-- ──────────────────────────────────────────────
buildDecoder :: String -> Double -> Double -> [Int]
             -> Build (LayoutComp, DecoderRefs)
buildDecoder name x y opBits = do
  cid <- freshComp
  invResults <- forM (zip opBits [0..7]) $ \(wOp, i) ->
    lgate "NOT" (name++"_not"++show (i::Int)) 10 (8+d i*16) 4 3 [wOp]
  let (invGates, opInvs) = unzip invResults
  let det pat yp = do
        let ins = [ if (pat `div` (2^(b::Int))) `mod` 2 == 1
                    then opBits!!b else opInvs!!b
                  | b <- [0..3] ]
        (lg, w) <- lgate "AND" (name++"_det"++show pat) 50 yp 5 3 ins
        return (lg, w)
  detResults <- sequence
    [ det 0x1 5,  det 0x2 15, det 0x3 25, det 0x4 35
    , det 0x5 45, det 0x6 55, det 0x7 65, det 0x8 75
    , det 0x9 85, det 0xA 95, det 0xB 105, det 0xC 115
    , det 0xF 135 ]
  let detGates = map fst detResults
      dw        = map snd detResults
      isLoadA   = dw!!0;  isLoadB  = dw!!1;  isLoadAM = dw!!2;  isStoreA = dw!!3
      isAdd     = dw!!4;  isSub    = dw!!5;  isAnd    = dw!!6;  isOr     = dw!!7
      isXor     = dw!!8;  isNot    = dw!!9;  isJmp    = dw!!10; isJz     = dw!!11
      isHlt     = dw!!12
  let ctrl srcs yp = case srcs of
        [w] -> return (Nothing, w)
        _   -> do (lg, w) <- lgate "OR" (name++"_ctrl") 130 yp 5 3 srcs
                  return (Just lg, w)
  ctrlResults <- sequence
    [ ctrl [isLoadA,isLoadB,isLoadAM,isAdd,isSub,isAnd,isOr,isXor,isNot] 10
    , ctrl [isLoadAM] 25
    , ctrl [isStoreA] 35
    , ctrl [isAnd, isXor] 55
    , ctrl [isOr,  isXor] 65
    , ctrl [isSub] 75
    , ctrl [isJmp] 95
    , ctrl [isJz]  105
    , ctrl [isHlt] 135
    , ctrl [isLoadA, isLoadB] 145
    , ctrl [isLoadB] 155
    ]
  let ctrlGates = [lg | (Just lg, _) <- ctrlResults]
      cw         = map snd ctrlResults
      wRegWrite  = cw!!0;  wMemRead  = cw!!1;  wMemWrite = cw!!2
      wAluOp0    = cw!!3;  wAluOp1   = cw!!4;  wSubMode  = cw!!5
      wBranch    = cw!!6;  wBranchZ  = cw!!7;  wHalt     = cw!!8
      wLoadImm   = cw!!9;  wRegDstB  = cw!!10
  let refs = DecoderRefs wRegWrite wMemRead wMemWrite wAluOp0 wAluOp1
               wSubMode wBranch wBranchZ wHalt wLoadImm wRegDstB
      ins  = zipWith (\w i -> ("Op"++show (i::Int), LayoutPin w 0 (10+d i*16))) opBits [0..7]
      outs = [("RegWrite",LayoutPin wRegWrite 200 10),("MemRead",LayoutPin wMemRead 200 25)
             ,("MemWrite",LayoutPin wMemWrite 200 35),("AluOp0",LayoutPin wAluOp0 200 55)
             ,("AluOp1",LayoutPin wAluOp1 200 65),("SubMode",LayoutPin wSubMode 200 75)
             ,("Branch",LayoutPin wBranch 200 95),("BranchZ",LayoutPin wBranchZ 200 105)
             ,("Halt",LayoutPin wHalt 200 135)]
      allGates = invGates ++ detGates ++ ctrlGates
      -- ワイヤセグメント用定数
      andYs   = [5,15,25,35,45,55,65,75,85,95,105,115,135] :: [Double]
      decPats = [0x1,0x2,0x3,0x4,0x5,0x6,0x7,0x8,0x9,0xA,0xB,0xC,0xF] :: [Int]
      xDb b   = 19 + d b * 3   -- 直接バスのx座標 (bit 0-3)
      xIb b   = 33 + d b * 3   -- 反転バスのx座標 (bit 0-3)
      yIn b   = 10 + d b * 16  -- 入力ピンのy座標
      yNot b  = 8  + d b * 16 + 1.5  -- NOTゲート中心y
      -- 入力→NOTゲート
      segsNot = [ (opBits!!b, [LayoutSeg 0 (yIn b) 10 (yNot b) cid])
                | b <- [0..7] ]
      -- 直接バス (bit 0-3): 入力ピン → 垂直バス
      segsDirBus = [ (opBits!!b,
                      [ LayoutSeg 0 (yIn b) (xDb b) (yIn b) cid
                      , LayoutSeg (xDb b) (yIn b) (xDb b) 138 cid ])
                   | b <- [0..3] ]
      -- 反転バス (bit 0-3): NOTゲート出力 → 垂直バス
      segsInvBus = [ (opInvs!!b,
                      [ LayoutSeg 14 (yNot b) (xIb b) (yNot b) cid
                      , LayoutSeg (xIb b) (yNot b) (xIb b) 138 cid ])
                   | b <- [0..3] ]
      -- バス → ANDゲート入力 (パターンに応じて直接/反転バスから)
      segsAndIn = concat
        [ [ let bit = (pat `div` (2^(b::Int))) `mod` 2
                xb  = if bit==1 then xDb b else xIb b
                wid = if bit==1 then opBits!!b else opInvs!!b
            in (wid, [LayoutSeg xb (ay+1) 50 (ay+1) cid])
          | b <- [0..3] ]
        | (pat, ay) <- zip decPats andYs ]
      -- ANDゲート → ORゲート接続 (i=ANDインデックス, oy=OR gate y)
      aoIdxs = [ (0::Int,10::Double),(1,10),(2,10),(4,10),(5,10)
               , (6,10),(7,10),(8,10),(9,10)
               , (6,55),(8,55),(7,65),(8,65)
               , (0,145),(1,145) ]
      segsAndOr = [ (dw!!i, [ LayoutSeg 55 (andYs!!i+1.5) 68 (andYs!!i+1.5) cid
                             , LayoutSeg 68 (andYs!!i+1.5) 68 (oy+1.5) cid
                             , LayoutSeg 68 (oy+1.5) 130 (oy+1.5) cid ])
                  | (i, oy) <- aoIdxs ]
      -- ORゲート出力 → 出力ピン
      segsOrOut = [ (cw!!0, [LayoutSeg 135 11.5 200 10 cid])
                  , (cw!!3, [LayoutSeg 135 56.5 200 55 cid])
                  , (cw!!4, [LayoutSeg 135 66.5 200 65 cid]) ]
      -- ANDゲート → 出力ピン直結 (ctrl単入力, i=ANDインデックス, py=ピンy)
      apIdxs = [ (2::Int,25::Double),(3,35),(5,75),(10,95),(11,105),(12,135) ]
      segsAndPin = [ (dw!!i, [ LayoutSeg 55 (andYs!!i+1.5) 165 (andYs!!i+1.5) cid
                              , LayoutSeg 165 (andYs!!i+1.5) 165 py cid
                              , LayoutSeg 165 py 200 py cid ])
                   | (i, py) <- apIdxs ]
      allSegs = segsNot ++ segsDirBus ++ segsInvBus ++ segsAndIn
             ++ segsAndOr ++ segsOrOut ++ segsAndPin
  let comp = LayoutComp cid "DECODER" name "Instruction Decoder" x y 200 160 "#fdcb6e"
               [] allGates allSegs ins outs 0
  return (comp, refs)

-- ──────────────────────────────────────────────
-- メモリユニット (可視化用)
-- ──────────────────────────────────────────────
buildMemory :: String -> Double -> Double
            -> Build (LayoutComp, [Int], [Int], [Int], Int, Int)
buildMemory name x y = do
  cid <- freshComp
  addrIns <- mapM (\_ -> freshWire) [0..7::Int]
  datIns  <- mapM (\_ -> freshWire) [0..7::Int]
  datOuts <- mapM (\_ -> freshWire) [0..7::Int]
  wRead   <- freshWire
  wWrite  <- freshWire
  cellGates <- forM [(r,c) | r<-[0..3::Int], c<-[0..3::Int]] $ \(r,c) -> do
    wDummy <- freshWire
    (lg, _) <- lgate "AND" (name++"_cell_"++show r++"_"++show c)
                 (30+d c*30) (30+d r*50) 20 12 [wDummy]
    return lg
  let ins  = zipWith (\w i -> ("Addr"++show (i::Int), LayoutPin w 0 (10+d i*14))) addrIns [0..7]
          ++ zipWith (\w i -> ("Din"++show (i::Int),  LayoutPin w 0 (130+d i*14))) datIns [0..7]
          ++ [("Read",LayoutPin wRead 80 0),("Write",LayoutPin wWrite 100 0)]
      outs = zipWith (\w i -> ("Dout"++show (i::Int), LayoutPin w 160 (10+d i*14))) datOuts [0..7]
  let comp = LayoutComp cid "MEMORY" name "Memory (256 bytes)" x y 160 260 "#6c5ce7"
               [] cellGates [] ins outs 0
  return (comp, addrIns, datIns, datOuts, wRead, wWrite)

-- ──────────────────────────────────────────────
-- プログラムカウンタ
-- ──────────────────────────────────────────────
buildPC :: String -> Double -> Double -> [Int] -> [Int] -> Int -> Int
        -> Build (LayoutComp, [Int])
buildPC name x y wNexts wTargets wBranch wCLK = do
  cid <- freshComp
  muxOuts <- forM (zip3 wNexts wTargets [0..7]) $ \(wNext, wTarget, i) -> do
    (_, wOut) <- buildMux2 (name++"_mux"++show (i::Int)) 70 (10+d i*28) wNext wTarget wBranch
    return wOut
  (reg, wQs) <- buildRegister8 (name++"_reg") 10 5 muxOuts wCLK
  let ins  = zipWith (\w i -> ("Next"++show (i::Int),   LayoutPin w 0 (10+d i*28))) wNexts   [0..7]
          ++ zipWith (\w i -> ("Target"++show (i::Int), LayoutPin w 0 (20+d i*28))) wTargets [0..7]
          ++ [("Branch",LayoutPin wBranch 50 260),("CLK",LayoutPin wCLK 65 0)]
      outs = zipWith (\w i -> ("Q"++show (i::Int), LayoutPin w 130 (10+d i*28))) wQs [0..7]
  let comp = LayoutComp cid "PC" name "Program Counter" x y 130 260 "#e84393"
               [reg] [] [] ins outs 0
  return (comp, wQs)

-- ──────────────────────────────────────────────
-- CPU全体の構築
-- ──────────────────────────────────────────────
data CPUBuildResult = CPUBuildResult
  { cbrLayout :: !LayoutComp
  , cbrGates  :: ![Gate]
  , cbrRefs   :: !CPURefs
  } deriving Show

buildCPU :: CPUBuildResult
buildCPU = fst $ runBuild go
  where
  go = do
    wCLK     <- freshWire
    wSel0    <- freshWire
    wSel1    <- freshWire
    wSubMode <- freshWire
    wBranch  <- freshWire
    regADins <- mapM (\_ -> freshWire) [0..7::Int]
    regBDins <- mapM (\_ -> freshWire) [0..7::Int]
    irDins   <- mapM (\_ -> freshWire) [0..7::Int]
    irLDins  <- mapM (\_ -> freshWire) [0..7::Int]
    decOpIns <- mapM (\_ -> freshWire) [0..7::Int]
    aluAIns  <- mapM (\_ -> freshWire) [0..7::Int]
    aluBIns  <- mapM (\_ -> freshWire) [0..7::Int]
    pcNexts  <- mapM (\_ -> freshWire) [0..7::Int]
    pcTargets<- mapM (\_ -> freshWire) [0..7::Int]
    (regA, regAOuts) <- buildRegister8 "RegA" 200 280 regADins wCLK
    (regB, regBOuts) <- buildRegister8 "RegB" 320 280 regBDins wCLK
    (ir,   irOuts)   <- buildRegister8 "IR_H" 700 50  irDins   wCLK
    (irL,  irLOuts)  <- buildRegister8 "IR_L" 820 50  irLDins  wCLK
    (dec,  decRefs)  <- buildDecoder   "DEC"  450 50  decOpIns
    (mem, memAddrs, _memDatIns, memDatOuts, wMemRead, wMemWrite)
                     <- buildMemory "MEM" 200 50
    (pc, pcOuts)     <- buildPC "PC" 50 50 pcNexts pcTargets wBranch wCLK
    (alu, aluOuts, wZero, wCout)
                     <- buildALU "ALU" 450 280 aluAIns aluBIns wSel0 wSel1 wSubMode
    s <- get
    let refs = CPURefs
          { crRegAIns    = regADins,  crRegAOuts   = regAOuts
          , crRegBIns    = regBDins,  crRegBOuts   = regBOuts
          , crPCOuts     = pcOuts
          , crIRIns      = irDins,    crIROuts     = irOuts
          , crIRLIns     = irLDins,   crIRLOuts    = irLOuts
          , crDecOpIns   = decOpIns,  crDecOuts    = decRefs
          , crALUAIns    = aluAIns,   crALUBIns    = aluBIns
          , crALUOp0     = wSel0,     crALUOp1     = wSel1
          , crALUSub     = wSubMode
          , crALUResults = aluOuts,   crALUZero    = wZero
          , crALUCout    = wCout
          , crMemAddrIns = memAddrs,  crMemDatOuts = memDatOuts
          , crMemRead    = wMemRead,  crMemWrite   = wMemWrite
          }
    cid <- freshComp
    let cpu = LayoutComp cid "CPU" "CPU" "CPU" 0 0 1200 800 "#2d3436"
                [regA{lcLabel="Register A"}, regB{lcLabel="Register B"}
                ,ir{lcLabel="IR (Opcode)"}, irL{lcLabel="IR (Operand)"}
                ,dec, mem, pc, alu]
                [] [] [] [] 0
    return $ CPUBuildResult cpu (bsGates s) refs
