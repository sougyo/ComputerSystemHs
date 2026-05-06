{-# LANGUAGE ForeignFunctionInterface #-}
-- WASM リアクターモード用エントリ (コンパイラ)。
-- hs_compile : ソースコード → アセンブリ文字列 (エラー時は "ERROR: ...")
-- hs_parse_ast : ソースコード → AST の Show 文字列 (学習用)
module Main where

import Foreign.C.String (CString, newCString, peekCString)
import Compiler.Parser (parseProgram)
import Compiler.Codegen (compile)

foreign export ccall "hs_compile" hsCompile :: CString -> IO CString
hsCompile cstr = do
  src <- peekCString cstr
  case parseProgram "<input>" src of
    Left  err -> newCString ("ERROR: " ++ show err)
    Right ast -> newCString (unlines (compile ast))

foreign export ccall "hs_parse_ast" hsParseAst :: CString -> IO CString
hsParseAst cstr = do
  src <- peekCString cstr
  case parseProgram "<input>" src of
    Left  err -> newCString ("ERROR: " ++ show err)
    Right ast -> newCString (show ast)

main :: IO ()
main = return ()
