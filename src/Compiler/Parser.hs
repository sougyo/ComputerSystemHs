-- | シンプルな C 風言語のパーサー。
--
-- ファイルは大きく2部構成:
--   1. 字句解析 (Lexer)  -- 空白・コメントのスキップ、トークン認識
--   2. 構文解析 (Parser) -- トークン列 → AST
--
-- 演算子の優先順位 (低 → 高):
--   ||  (論理OR)
--   &&  (論理AND)
--   == != < > <= >=  (比較)
--   + -  (加減算)
--   * /  (乗除算)
--   単項マイナス -
--   一次式 (リテラル / 変数 / 関数呼び出し / カッコ)
module Compiler.Parser
    ( parseProgram
    , ParseError
    ) where

import Compiler.AST
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
import Text.Parsec.Language (emptyDef)

-- ===========================================================================
-- 字句解析 (Lexer)
-- ===========================================================================

-- | 言語の字句規則を定義する。
-- Parsec はこの定義から空白スキップや予約語チェックを自動生成する。
langDef :: Tok.LanguageDef ()
langDef = emptyDef
    { Tok.commentStart    = "/*"           -- ブロックコメント開始
    , Tok.commentEnd      = "*/"           -- ブロックコメント終了
    , Tok.commentLine     = "//"           -- 行コメント
    , Tok.identStart      = letter   <|> char '_'
    , Tok.identLetter     = alphaNum <|> char '_'
    , Tok.reservedNames   = [ "while", "if", "else", "return"
                            , "int", "float", "void", "string"
                            ]
    , Tok.reservedOpNames = [ "+",  "-",  "*",  "/"
                            , "<=", ">=", "==", "!=", "<", ">"
                            , "&&", "||"
                            , "="
                            ]
    , Tok.caseSensitive   = True
    }

-- | langDef からトークンパーサー群を生成する。
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

-- よく使うトークンパーサーに短い別名をつける。
-- Parsec の TokenParser は空白・コメントを自動でスキップする。

integer    :: Parser Integer
integer    = Tok.integer       lexer

float      :: Parser Double
float      = Tok.float         lexer

strLit     :: Parser String
strLit     = Tok.stringLiteral lexer

ident      :: Parser String
ident      = Tok.identifier    lexer

reserved   :: String -> Parser ()
reserved   = Tok.reserved      lexer

reservedOp :: String -> Parser ()
reservedOp = Tok.reservedOp    lexer

parens     :: Parser a -> Parser a
parens     = Tok.parens        lexer   -- '(' ... ')'

braces     :: Parser a -> Parser a
braces     = Tok.braces        lexer   -- '{' ... '}'

semi       :: Parser String
semi       = Tok.semi          lexer   -- ';'

comma      :: Parser String
comma      = Tok.comma         lexer   -- ','

ws         :: Parser ()
ws         = Tok.whiteSpace    lexer   -- 空白・コメントをスキップ

-- ===========================================================================
-- 型パーサー
-- ===========================================================================

pType :: Parser Type
pType = choice
    [ reserved "int"    *> pure TInt
    , reserved "float"  *> pure TFloat
    , reserved "void"   *> pure TVoid
    , reserved "string" *> pure TString
    ]

-- ===========================================================================
-- 式パーサー  (優先順位: 低 → 高 の順に定義)
-- ===========================================================================

-- | 式のエントリーポイント。最も優先順位の低い || から始める。
pExpr :: Parser Expr
pExpr = pOr

-- | || は最低優先順位。左結合: a || b || c = (a || b) || c
pOr :: Parser Expr
pOr = chainl1 pAnd (reservedOp "||" *> pure (EBin Or))

-- | && は || より高優先。左結合。
pAnd :: Parser Expr
pAnd = chainl1 pCmp (reservedOp "&&" *> pure (EBin And))

-- | 比較演算子は非結合 (a < b < c はパースエラー)。
pCmp :: Parser Expr
pCmp = do
    left <- pAdd
    -- 比較演算子がなければ left をそのまま返す (option)
    option left $ do
        op    <- pCmpOp
        right <- pAdd
        return (EBin op left right)

pCmpOp :: Parser BinOp
pCmpOp = choice
    [ reservedOp "<=" *> pure Le   -- <= を < より先にマッチさせる
    , reservedOp ">=" *> pure Ge
    , reservedOp "==" *> pure Eq
    , reservedOp "!=" *> pure Ne
    , reservedOp "<"  *> pure Lt
    , reservedOp ">"  *> pure Gt
    ]

-- | + と - は左結合。
pAdd :: Parser Expr
pAdd = chainl1 pMul $ choice
    [ reservedOp "+" *> pure (EBin Add)
    , reservedOp "-" *> pure (EBin Sub)
    ]

-- | * と / は左結合、+ より高優先。
pMul :: Parser Expr
pMul = chainl1 pUnary $ choice
    [ reservedOp "*" *> pure (EBin Mul)
    , reservedOp "/" *> pure (EBin Div)
    ]

-- | 単項マイナスは右結合: --x = -(-x)
pUnary :: Parser Expr
pUnary = (reservedOp "-" *> (ENeg <$> pUnary))
      <|> pPrimary

-- | 一次式: 式木の葉ノードになるもの
pPrimary :: Parser Expr
pPrimary = choice
    [ try pFloatLit  -- 整数より先に試す ("3.14" を "3" と ".14" に分けないため)
    , pIntLit
    , pStrLit
    , try pCall      -- 変数より先に試す (どちらも識別子から始まるため)
    , pVar
    , parens pExpr   -- カッコによるグループ化
    ]

pIntLit :: Parser Expr
pIntLit = ELit . LInt . fromIntegral <$> integer

pFloatLit :: Parser Expr
pFloatLit = ELit . LFloat <$> float

pStrLit :: Parser Expr
pStrLit = ELit . LString <$> strLit

pVar :: Parser Expr
pVar = EVar <$> ident

-- | 関数呼び出し: name '(' arg, arg, ... ')'
pCall :: Parser Expr
pCall = do
    name <- ident
    args <- parens (pExpr `sepBy` comma)
    return (ECall name args)

-- ===========================================================================
-- 文パーサー
-- ===========================================================================

pStmt :: Parser Stmt
pStmt = choice
    [ pWhile
    , pIf
    , pReturn
    , try pAssign   -- 代入と式文は識別子で始まるため try で試す
    , pExprStmt
    ]

-- | 代入文: ident '=' expr
-- '==' との区別は reservedOp が自動で行う
pAssign :: Parser Stmt
pAssign = do
    name <- ident
    reservedOp "="
    expr <- pExpr
    return (SAssign name expr)

-- | 式文: 関数呼び出しなどを文として使う
pExprStmt :: Parser Stmt
pExprStmt = SExpr <$> pExpr

-- | while ループ: while '(' cond ')' '{' stmts '}'
pWhile :: Parser Stmt
pWhile = do
    reserved "while"
    cond  <- parens pExpr
    stmts <- braces pStmts
    return (SWhile cond stmts)

-- | if 文: if '(' cond ')' '{' stmts '}' [ else '{' stmts '}' ]
pIf :: Parser Stmt
pIf = do
    reserved "if"
    cond  <- parens pExpr
    thenB <- braces pStmts
    elseB <- option [] (reserved "else" *> braces pStmts)
    return (SIf cond thenB elseB)

-- | return 文: return expr
pReturn :: Parser Stmt
pReturn = reserved "return" *> (SReturn <$> pExpr)

-- | ブロック内の文の列。セミコロン区切り (末尾のセミコロンは省略可)。
pStmts :: Parser [Stmt]
pStmts = pStmt `sepEndBy` semi

-- ===========================================================================
-- 関数定義パーサー
-- ===========================================================================

-- | 関数の引数: type ident
pParam :: Parser Param
pParam = Param <$> pType <*> ident

-- | 関数定義: type name '(' params ')' '{' stmts '}'
pFunction :: Parser Function
pFunction = do
    retType <- pType
    name    <- ident
    params  <- parens (pParam `sepBy` comma)
    body    <- braces pStmts
    return (Function retType name params body)

-- ===========================================================================
-- エントリーポイント
-- ===========================================================================

-- | ソースコードをパースしてプログラムの AST を返す。
parseProgram :: String            -- ^ ソース名 (エラーメッセージに使用)
             -> String            -- ^ ソースコード
             -> Either ParseError Program
parseProgram srcName src = parse program srcName src
  where
    program = ws *> many pFunction <* eof
