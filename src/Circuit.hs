{-# LANGUAGE StrictData #-}
module Circuit
  ( GateType(..)
  , Gate(..)
  , WireVals
  , evalGateType
  , evalOnePass
  , evaluate
  , setWire
  , getWire
  , setWires8
  , getWires8
  ) where

import Data.Bits (shiftR, shiftL, (.&.))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')

data GateType = AND | OR | NAND | NOR | XOR | XNOR | NOT | BUF
  deriving (Show, Eq, Ord, Enum, Bounded)

-- 論理ゲート: 入力/出力はワイヤID
data Gate = Gate
  { gType :: !GateType
  , gIns  :: ![Int]
  , gOut  :: !Int
  } deriving (Show, Eq)

-- 全ワイヤの値 (ワイヤID → Bool)
type WireVals = IntMap Bool

evalGateType :: GateType -> [Bool] -> Bool
evalGateType AND  xs       = and xs
evalGateType OR   xs       = or xs
evalGateType NAND xs       = not (and xs)
evalGateType NOR  xs       = not (or xs)
evalGateType XOR  [a, b]   = a /= b
evalGateType XNOR [a, b]   = a == b
evalGateType NOT  [a]      = not a
evalGateType BUF  [a]      = a
evalGateType _    _        = False

evalOnePass :: [Gate] -> WireVals -> WireVals
evalOnePass gs vs = foldl' step vs gs
  where
    step ws g =
      let ins = map (\i -> IM.findWithDefault False i ws) (gIns g)
          out = evalGateType (gType g) ins
      in  IM.insert (gOut g) out ws

-- 安定するまで繰り返し評価 (フリップフロップのフィードバック対応)
evaluate :: [Gate] -> WireVals -> WireVals
evaluate gs = go 20
  where
    go 0 vs = vs
    go n vs =
      let vs' = evalOnePass gs vs
      in  if vs' == vs then vs' else go (n - 1) vs'

setWire :: Int -> Bool -> WireVals -> WireVals
setWire = IM.insert

getWire :: Int -> WireVals -> Bool
getWire wid vs = IM.findWithDefault False wid vs

setWires8 :: [Int] -> Int -> WireVals -> WireVals
setWires8 wids val vs =
  foldl' (\acc (wid, bit) -> IM.insert wid (((val `shiftR` bit) .&. 1) == 1) acc)
         vs (zip wids [0 .. 7])

getWires8 :: [Int] -> WireVals -> Int
getWires8 wids vs =
  sum [ if IM.findWithDefault False wid vs then 1 `shiftL` i else 0
      | (wid, i) <- zip wids [0 .. 7] ]
