-- | AST → ComputerSystemHs 命令セット コード生成器
--
-- 対応命令セット (ComputerSystemHs の拡張後):
--   NOP / LOAD_A imm / LOAD_B imm / LOAD_A_MEM addr / LOAD_B_MEM addr / STORE_A addr
--   ADD / SUB / AND / OR / XOR / NOT / SHL / MUL
--   JMP addr / JZ addr / JNZ addr / CALL addr / RET / HLT
module Compiler.Codegen (compile) where

import Compiler.AST
import Data.Word (Word8)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad.State
import Data.List (partition)
import Numeric (showHex)

-- ── 中間表現 ──────────────────────────────────────

type LblName = String

data Instr
  = INop
  | ILoadA    Word8
  | ILoadB    Word8
  | ILoadAMem Word8
  | ILoadBMem Word8      -- LOAD_B_MEM addr  (B ← mem[addr])
  | IStoreA   Word8
  | IAdd | ISub | IAnd | IOr | IXor | INot | IShl | IMul
  | IJmp  LblName
  | IJz   LblName
  | IJnz  LblName
  | ICall LblName        -- CALL addr  (PUSH PC+2; JMP addr)
  | IRet                 -- RET        (POP → PC)
  | IHlt
  deriving (Show, Eq)

data Line
  = LCode    Instr
  | LLabel   LblName
  | LComment String
  deriving Show

-- ── コード生成ステート ─────────────────────────────

data CGS = CGS
  { cgsVars   :: Map String Word8    -- 変数名 → メモリアドレス
  , cgsNextV  :: Word8               -- 次の空きアドレス (0x80 から)
  , cgsLblCtr :: Int                 -- ラベル番号カウンタ
  , cgsFuncs  :: Map String [String] -- 関数名 → パラメータ名リスト
  , cgsIsMain :: Bool                -- main 関数コンパイル中か
  } deriving Show

initialCGS :: CGS
initialCGS = CGS
  { cgsVars   = Map.empty
  , cgsNextV  = 0x80
  , cgsLblCtr = 0
  , cgsFuncs  = Map.empty
  , cgsIsMain = True
  }

type CG = State CGS

getAddr :: String -> CG Word8
getAddr name = do
  st <- get
  case Map.lookup name (cgsVars st) of
    Just a  -> return a
    Nothing -> do
      let a = cgsNextV st
      put st { cgsVars  = Map.insert name a (cgsVars st)
             , cgsNextV = a + 1 }
      return a

newLabel :: String -> CG LblName
newLabel pfx = do
  n <- gets cgsLblCtr
  modify (\s -> s { cgsLblCtr = n + 1 })
  return (pfx ++ show n)

-- ── エントリーポイント ────────────────────────────

compile :: Program -> [String]
compile fns =
  let (code0, st) = runState buildAll initialCGS
      lmap  = buildLabelMap code0
  in varHeader (cgsVars st) ++ resolveLines lmap code0
  where
    buildAll = do
      -- フェーズ1: 全関数のシグネチャ登録 (パラメータのアドレスを先に確保)
      mapM_ registerFunc fns
      -- フェーズ2: main と non-main に分けてコンパイル
      let (mains, others) = partition (\f -> funcName f == "main") fns
      -- non-main 関数が存在する場合のみ先頭に JMP main を挿入
      let prefix = [LCode (IJmp "main") | not (null others)]
      otherCode <- concat <$> mapM (compileFunc False) others
      mainCode  <- concat <$> mapM (compileFunc True)  mains
      return $ prefix ++ otherCode ++ mainCode

registerFunc :: Function -> CG ()
registerFunc fn = do
  let paramNames = map (\(Param _ n) -> n) (funcParams fn)
  mapM_ getAddr paramNames   -- アドレスを確保
  modify (\s -> s { cgsFuncs = Map.insert (funcName fn) paramNames (cgsFuncs s) })

varHeader :: Map String Word8 -> [String]
varHeader m
  | Map.null visible = []
  | otherwise  =
      "; === 変数レイアウト ==="
      : map (\(k, v) -> ";   " ++ k ++ " @ 0x" ++ pad2 v) (Map.toList visible)
      ++ [";"]
  where
    visible = Map.filterWithKey (\k _ -> take 2 k /= "__") m
    pad2 n  = let s = showHex n "" in if length s < 2 then '0':s else s

-- ── 関数コンパイル ────────────────────────────────

compileFunc :: Bool -> Function -> CG [Line]
compileFunc isMain fn = do
  modify (\s -> s { cgsIsMain = isMain })
  let comment  = LComment ("; === 関数: " ++ funcName fn ++ " ===")
      lbl      = LLabel (funcName fn)
  body <- concat <$> mapM compileStmt (funcBody fn)
  -- SReturn がない場合のフォールスルー防止
  let fallback = if isMain then [LCode IHlt] else [LCode IRet]
  return $ comment : lbl : body ++ fallback

-- ── 文コンパイル ──────────────────────────────────

compileStmt :: Stmt -> CG [Line]

compileStmt (SAssign name expr) = do
  addr  <- getAddr name
  eCode <- compileExpr expr
  return $ eCode ++ [LCode (IStoreA addr)]

compileStmt (SExpr expr) = compileExpr expr

compileStmt (SReturn expr) = do
  c <- compileExpr expr
  isMain <- gets cgsIsMain
  let term = if isMain then LCode IHlt else LCode IRet
  return $ LComment "; return (result in A)" : c ++ [term]

compileStmt (SWhile cond body) = do
  loopLbl <- newLabel "loop"
  endLbl  <- newLabel "end"
  condCode <- compileCondExit cond endLbl
  bodyCode <- concat <$> mapM compileStmt body
  return $
    LLabel loopLbl :
    condCode ++
    bodyCode ++
    [LCode (IJmp loopLbl), LLabel endLbl]

-- if (cond) { then } — else なし
compileStmt (SIf cond thenStmts []) = do
  endLbl   <- newLabel "endif"
  condCode <- compileCondExit cond endLbl
  thenCode <- concat <$> mapM compileStmt thenStmts
  return $ condCode ++ thenCode ++ [LLabel endLbl]

-- if (cond) { then } else { else }
compileStmt (SIf cond thenStmts elseStmts) = do
  elseLbl  <- newLabel "else"
  endLbl   <- newLabel "endif"
  condCode <- compileCondExit cond elseLbl
  thenCode <- concat <$> mapM compileStmt thenStmts
  elseCode <- concat <$> mapM compileStmt elseStmts
  return $ condCode ++ thenCode ++ [LCode (IJmp endLbl)]
        ++ [LLabel elseLbl] ++ elseCode ++ [LLabel endLbl]

-- ── 条件コンパイル (false のとき lbl へジャンプ) ────

compileCondExit :: Expr -> LblName -> CG [Line]

compileCondExit (EBin Ne l r) lbl = do
  c <- compileSubAB l r
  return $ c ++ [LCode (IJz lbl)]

compileCondExit (EBin Eq l r) lbl = do
  c <- compileSubAB l r
  return $ c ++ [LCode (IJnz lbl)]

compileCondExit (EBin Gt l r) lbl = do
  c <- compileSubAB l r         -- A = l - r; l>r なら正
  return $ c ++ [LCode (IJz lbl)]   -- 0 なら l==r → exit

compileCondExit (EBin Lt l r) lbl = do
  c <- compileSubAB r l
  return $ c ++ [LCode (IJz lbl)]

compileCondExit (EBin Le l r) lbl = do
  c <- compileSubAB r l
  return $ c ++ [LCode (IJnz lbl)]

compileCondExit (EBin Ge l r) lbl = do
  c <- compileSubAB l r
  return $ c ++ [LCode (IJnz lbl)]

compileCondExit (EVar name) lbl = do
  addr <- getAddr name
  return [LCode (ILoadAMem addr), LCode (IJz lbl)]

compileCondExit expr lbl = do
  c <- compileExpr expr
  return $ c ++ [LCode (IJz lbl)]

-- ── 減算: A = l - r ──────────────────────────────

compileSubAB :: Expr -> Expr -> CG [Line]

-- 右辺が即値
compileSubAB l (ELit (LInt n)) = do
  lc <- compileExpr l
  return $ lc ++ [LCode (ILoadB (toW8 n)), LCode ISub]

-- 左辺が即値 (n - r = -(r-n) を 2の補数で表現)
compileSubAB (ELit (LInt n)) r = do
  rc <- compileExpr r
  return $ rc
        ++ [LCode INot, LCode (ILoadB 1), LCode IAdd]   -- A = -r
        ++ [LCode (ILoadB (toW8 n)), LCode IAdd]          -- A = n + (-r)

-- 変数 - 変数: LOAD_B_MEM を使用
compileSubAB (EVar x) (EVar y) = do
  ax <- getAddr x
  ay <- getAddr y
  return [LCode (ILoadAMem ax), LCode (ILoadBMem ay), LCode ISub]

-- 汎用ケース: テンポラリ変数にスピル
compileSubAB l r = do
  tmpLbl  <- newLabel "__st"
  tmpAddr <- getAddr tmpLbl
  rc <- compileExpr r
  lc <- compileExpr l
  -- mem[tmp] = r の結果; A = l の結果; B = r; SUB → l - r
  return $ rc ++ [LCode (IStoreA tmpAddr)]
        ++ lc ++ [LCode (ILoadBMem tmpAddr), LCode ISub]

-- ── 式コンパイル (結果は A レジスタ) ─────────────

compileExpr :: Expr -> CG [Line]

compileExpr (ELit (LInt n)) = return [LCode (ILoadA (toW8 n))]

compileExpr (ELit (LFloat _)) =
  return [LComment "; ERROR: float 未サポート", LCode INop]

compileExpr (ELit (LString _)) =
  return [LComment "; ERROR: string 未サポート", LCode INop]

compileExpr (EVar name) = do
  addr <- getAddr name
  return [LCode (ILoadAMem addr)]

compileExpr (ENeg e) = do
  c <- compileExpr e
  return $ c ++ [LCode INot, LCode (ILoadB 1), LCode IAdd]

compileExpr (ECall name args) = do
  funcs <- gets cgsFuncs
  case Map.lookup name funcs of
    Nothing ->
      return [LComment ("; ERROR: 未定義の関数 '" ++ name ++ "'"), LCode INop]
    Just params
      | length params /= length args ->
          return [LComment ("; ERROR: 引数の数が合わない: " ++ name), LCode INop]
      | otherwise -> do
          -- 各引数を対応するパラメータのアドレスに格納してから CALL
          storeCode <- concat <$>
            mapM (\(pName, aExpr) -> do
                    pAddr  <- getAddr pName
                    aCode  <- compileExpr aExpr
                    return $ aCode ++ [LCode (IStoreA pAddr)]
                 ) (zip params args)
          return $ storeCode ++ [LCode (ICall name)]

compileExpr (EBin op l r) = compileBin op l r

-- ── 二項演算コンパイル ────────────────────────────

compileBin :: BinOp -> Expr -> Expr -> CG [Line]

-- 右辺が即値
compileBin op l (ELit (LInt n)) | isALU op = do
  lc <- compileExpr l
  return $ lc ++ [LCode (ILoadB (toW8 n)), LCode (aluOf op)]

-- 右辺が変数 (LOAD_B_MEM を使用)
compileBin op l (EVar y) | isALU op = do
  lc <- compileExpr l
  ay <- getAddr y
  return $ lc ++ [LCode (ILoadBMem ay), LCode (aluOf op)]

-- 左辺が即値 + 交換則あり
compileBin op (ELit (LInt n)) r | isALU op && isComm op = do
  rc <- compileExpr r
  return $ rc ++ [LCode (ILoadB (toW8 n)), LCode (aluOf op)]

-- 左辺が変数 + 交換則あり (LOAD_B_MEM を使用)
compileBin op (EVar x) r | isALU op && isComm op = do
  rc <- compileExpr r
  ax <- getAddr x
  return $ rc ++ [LCode (ILoadBMem ax), LCode (aluOf op)]

-- MUL の汎用ケース (テンポラリを使用)
compileBin Mul l r = do
  tmpLbl  <- newLabel "__mt"
  tmpAddr <- getAddr tmpLbl
  lc <- compileExpr l
  rc <- compileExpr r
  -- mem[tmp] = l の結果; A = r の結果; B = l; MUL → l * r
  return $ lc ++ [LCode (IStoreA tmpAddr)]
        ++ rc ++ [LCode (ILoadBMem tmpAddr), LCode IMul]

-- 比較系: 差分で代替
compileBin Sub l r = compileSubAB l r
compileBin Eq  l r = compileSubAB l r
compileBin Ne  l r = compileSubAB l r
compileBin Gt  l r = compileSubAB l r
compileBin Lt  l r = compileSubAB r l
compileBin Le  l r = compileSubAB r l
compileBin Ge  l r = compileSubAB l r

-- 未サポート
compileBin op _ _ =
  return [LComment ("; ERROR: " ++ show op ++ " 一般ケース未サポート"), LCode INop]

-- ── ヘルパー ──────────────────────────────────────

isALU :: BinOp -> Bool
isALU Add = True; isALU Sub = True; isALU And = True; isALU Or = True; isALU Mul = True
isALU _   = False

isComm :: BinOp -> Bool
isComm Add = True; isComm And = True; isComm Or = True; isComm Mul = True; isComm _ = False

aluOf :: BinOp -> Instr
aluOf Add = IAdd; aluOf Sub = ISub; aluOf And = IAnd; aluOf Or = IOr; aluOf Mul = IMul
aluOf _   = INop

toW8 :: Int -> Word8
toW8 = fromIntegral

-- ── ラベル解決 ────────────────────────────────────

buildLabelMap :: [Line] -> Map LblName Int  -- label → 命令インデックス
buildLabelMap = snd . foldl step (0, Map.empty)
  where
    step (i, m) (LCode _)    = (i + 1, m)
    step (i, m) (LLabel lbl) = (i,     Map.insert lbl i m)
    step (i, m) (LComment _) = (i,     m)

resolveLines :: Map LblName Int -> [Line] -> [String]
resolveLines lmap = concatMap fmt
  where
    toAddr idx = fromIntegral (idx * 2) :: Word8

    resolve lbl = case Map.lookup lbl lmap of
      Just idx -> toAddr idx
      Nothing  -> 0

    fmt (LComment c)  = [c]
    fmt (LLabel  lbl) = [lbl ++ ":"]
    fmt (LCode instr) = [fmtInstr instr]

    fmtInstr INop           = "NOP"
    fmtInstr (ILoadA   n)   = "LOAD_A "     ++ show n
    fmtInstr (ILoadB   n)   = "LOAD_B "     ++ show n
    fmtInstr (ILoadAMem n)  = "LOAD_A_MEM " ++ show n
    fmtInstr (ILoadBMem n)  = "LOAD_B_MEM " ++ show n
    fmtInstr (IStoreA  n)   = "STORE_A "    ++ show n
    fmtInstr IAdd           = "ADD"
    fmtInstr ISub           = "SUB"
    fmtInstr IAnd           = "AND"
    fmtInstr IOr            = "OR"
    fmtInstr IXor           = "XOR"
    fmtInstr INot           = "NOT"
    fmtInstr IShl           = "SHL"
    fmtInstr IMul           = "MUL"
    fmtInstr (IJmp lbl)     = "JMP "  ++ show (resolve lbl)
    fmtInstr (IJz  lbl)     = "JZ "   ++ show (resolve lbl)
    fmtInstr (IJnz lbl)     = "JNZ "  ++ show (resolve lbl)
    fmtInstr (ICall lbl)    = "CALL " ++ show (resolve lbl)
    fmtInstr IRet           = "RET"
    fmtInstr IHlt           = "HLT"
