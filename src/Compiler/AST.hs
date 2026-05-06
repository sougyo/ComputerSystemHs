-- | Abstract Syntax Tree for a simple C-like language.
module Compiler.AST where

-- ---------------------------------------------------------------------------
-- 型
-- ---------------------------------------------------------------------------

-- | 言語がサポートするプリミティブ型
data Type
    = TInt      -- ^ int
    | TFloat    -- ^ float
    | TVoid     -- ^ void (関数の戻り値のみ)
    | TString   -- ^ string
    deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- 式 (Expressions)
-- ---------------------------------------------------------------------------

-- | 二項演算子
data BinOp
    -- 算術演算
    = Add | Sub | Mul | Div
    -- 比較演算
    | Eq  | Ne  | Lt  | Gt  | Le  | Ge
    -- 論理演算
    | And | Or
    deriving (Show, Eq)

-- | ソースコード中に直接書かれたリテラル値
data Literal
    = LInt    Int     -- ^ 整数リテラル: 42
    | LFloat  Double  -- ^ 浮動小数点リテラル: 3.14
    | LString String  -- ^ 文字列リテラル: "hello"
    deriving (Show, Eq)

-- | 値を計算する式
data Expr
    = ELit  Literal           -- ^ リテラル
    | EVar  String            -- ^ 変数参照
    | EBin  BinOp Expr Expr   -- ^ 二項演算:  a + b
    | ECall String [Expr]     -- ^ 関数呼び出し: f(a, b)
    | ENeg  Expr              -- ^ 単項マイナス: -x
    deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- 文 (Statements)
-- ---------------------------------------------------------------------------

-- | 動作を実行する文
data Stmt
    = SAssign String Expr        -- ^ 代入文:    x = expr
    | SExpr   Expr               -- ^ 式文:      expr
    | SWhile  Expr [Stmt]        -- ^ while ループ: while (cond) { ... }
    | SIf     Expr [Stmt] [Stmt] -- ^ if 文: if (cond) { then } else { else }
    | SReturn Expr               -- ^ return 文: return expr
    deriving (Show, Eq)

-- ---------------------------------------------------------------------------
-- トップレベル宣言
-- ---------------------------------------------------------------------------

-- | 関数の引数 (型と変数名のペア)
data Param = Param Type String
    deriving (Show, Eq)

-- | トップレベルの関数定義
data Function = Function
    { funcRetType :: Type     -- ^ 戻り値の型
    , funcName    :: String   -- ^ 関数名
    , funcParams  :: [Param]  -- ^ 引数リスト
    , funcBody    :: [Stmt]   -- ^ 関数本体
    } deriving (Show, Eq)

-- | プログラムは関数定義のリスト
type Program = [Function]
