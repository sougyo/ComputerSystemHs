-- テキストアセンブリ → バイト列変換
module Assembler (assemble) where

import Data.Char (toUpper, isSpace)
import Data.Maybe (mapMaybe)
import Data.Word (Word8)

-- "LOAD_A 25\nADD\nHLT" → [(opcode, operand), ...]
assemble :: String -> [(Word8, Word8)]
assemble = mapMaybe parseLine . lines
  where
    parseLine line =
      let trimmed = map toUpper . dropWhile isSpace . takeWhile (/=';') $ line
      in  if null trimmed then Nothing
          else case words trimmed of
            []     -> Nothing
            (mn:rest) ->
              let op  = mnemonicToOpcode mn
                  arg = case rest of
                          (a:_) -> readNum a
                          []    -> 0
              in  Just (op, arg)

mnemonicToOpcode :: String -> Word8
mnemonicToOpcode m = case m of
  "NOP"       -> 0x00
  "LOAD_A"    -> 0x01
  "LOAD_B"    -> 0x02
  "LOAD_A_MEM"-> 0x03
  "STORE_A"   -> 0x04
  "ADD"       -> 0x05
  "SUB"       -> 0x06
  "AND"       -> 0x07
  "OR"        -> 0x08
  "XOR"       -> 0x09
  "NOT"       -> 0x0A
  "JMP"       -> 0x0B
  "JZ"        -> 0x0C
  "JNZ"       -> 0x0D
  "SHL"       -> 0x0E
  "HLT"       -> 0x0F
  _           -> 0x00

readNum :: String -> Word8
readNum s = case s of
  ('0':'x':hex) -> fromIntegral (hexToInt hex)
  ('0':'b':bin) -> fromIntegral (binToInt bin)
  dec           -> fromIntegral (read dec :: Int)

hexToInt :: String -> Int
hexToInt = foldl (\acc c -> acc * 16 + digitVal c) 0
  where
    digitVal c
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
      | c >= 'A' && c <= 'F' = 10 + fromEnum c - fromEnum 'A'
      | c >= 'a' && c <= 'f' = 10 + fromEnum c - fromEnum 'a'
      | otherwise = 0

binToInt :: String -> Int
binToInt = foldl (\acc c -> acc * 2 + if c == '1' then 1 else 0) 0
