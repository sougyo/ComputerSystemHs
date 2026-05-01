{-# LANGUAGE StrictData #-}
-- Builder モナド: ワイヤIDとゲートIDを管理しながら回路を構築する
module Builder
  ( BuildState(..)
  , Build
  , runBuild
  , freshWire
  , freshWireNamed
  , freshComp
  , addGate
  , addGateFixed  -- 出力ワイヤIDを事前指定するバリアント (フィードバック用)
  , topoSort
  ) where

import Control.Monad.State.Strict
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (foldl')

import Circuit (Gate(..), GateType(..))

data BuildState = BuildState
  { bsWireNext :: !Int
  , bsCompNext :: !Int
  , bsGates    :: ![Gate]
  , bsWireNames:: !(IntMap String)
  }

type Build a = State BuildState a

runBuild :: Build a -> (a, BuildState)
runBuild b = runState b (BuildState 0 0 [] IM.empty)

-- 新しいワイヤIDを払い出す
freshWire :: Build Int
freshWire = do
  s <- get
  let w = bsWireNext s
  put s { bsWireNext = w + 1 }
  return w

freshWireNamed :: String -> Build Int
freshWireNamed name = do
  s <- get
  let w = bsWireNext s
  put s { bsWireNext = w + 1
        , bsWireNames = IM.insert w name (bsWireNames s)
        }
  return w

freshComp :: Build Int
freshComp = do
  s <- get
  let c = bsCompNext s
  put s { bsCompNext = c + 1 }
  return c

-- ゲートを追加し、出力ワイヤIDを返す
addGate :: GateType -> [Int] -> Build Int
addGate gtype ins = do
  s <- get
  let out = bsWireNext s
      g   = Gate gtype ins out
  put s { bsWireNext = out + 1
        , bsGates    = bsGates s ++ [g]
        }
  return out

-- フィードバックループ用: 出力ワイヤIDを事前に指定
addGateFixed :: GateType -> [Int] -> Int -> Build ()
addGateFixed gtype ins out = do
  s <- get
  let g = Gate gtype ins out
  put s { bsGates = bsGates s ++ [g] }

-- ゲートリストをトポロジカル順にソート (Kahnのアルゴリズム)
topoSort :: [Gate] -> [Gate]
topoSort gates = sorted ++ unvisited
  where
    outToGate  = IM.fromList [(gOut g, g) | g <- gates]
    outSet     = IS.fromList $ map gOut gates

    -- あるゲートの先行ゲート (入力を生成するゲート)
    preds g = [ p | i <- gIns g, Just p <- [IM.lookup i outToGate], p /= g ]

    -- in-degree を計算
    inDeg = foldl' (\m g -> IM.insertWith (+) (gOut g) (length (preds g)) m) IM.empty gates

    roots = filter (\g -> all (`IS.notMember` outSet) (gIns g)) gates

    go [] vis acc = (acc, vis)
    go (g:queue) vis acc
      | IS.member (gOut g) vis = go queue vis acc
      | otherwise =
          let vis'  = IS.insert (gOut g) vis
              ready = [ g2 | g2 <- gates
                           , gOut g `elem` gIns g2
                           , IS.notMember (gOut g2) vis'
                           , all (`IS.member` (IS.insert (gOut g) (IS.fromList (map gOut acc)))) (filter (`IS.member` outSet) (gIns g2))
                           ]
          in go (queue ++ ready) vis' (acc ++ [g])

    (sorted, visited) = go roots IS.empty []
    unvisited = filter (\g -> IS.notMember (gOut g) visited) gates
