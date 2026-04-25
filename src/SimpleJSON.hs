-- JSON文字列ビルダー (外部パッケージ不要)
module SimpleJSON
  ( JVal(..)
  , renderJSON
  , jObj, jArr, jStr, jNum, jBool, jNull
  ) where

import Data.List (intercalate)
import Numeric (showHex)

data JVal
  = JObj  [(String, JVal)]
  | JArr  [JVal]
  | JStr  String
  | JNum  Double
  | JInt  Int
  | JBool Bool
  | JNull
  deriving Eq

jObj :: [(String, JVal)] -> JVal
jObj = JObj

jArr :: [JVal] -> JVal
jArr = JArr

jStr :: String -> JVal
jStr = JStr

jNum :: Double -> JVal
jNum = JNum

jBool :: Bool -> JVal
jBool = JBool

jNull :: JVal
jNull = JNull

renderJSON :: JVal -> String
renderJSON (JObj kvs) =
  "{" ++ intercalate "," (map renderPair kvs) ++ "}"
  where renderPair (k, v) = renderJSON (JStr k) ++ ":" ++ renderJSON v
renderJSON (JArr xs) =
  "[" ++ intercalate "," (map renderJSON xs) ++ "]"
renderJSON (JStr s)  = "\"" ++ escapeStr s ++ "\""
renderJSON (JNum d)  =
  if d == fromIntegral (round d :: Int)
    then show (round d :: Int)
    else show d
renderJSON (JInt n)  = show n
renderJSON (JBool b) = if b then "true" else "false"
renderJSON JNull     = "null"

escapeStr :: String -> String
escapeStr = concatMap esc
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\r' = "\\r"
    esc '\t' = "\\t"
    esc c    = [c]
