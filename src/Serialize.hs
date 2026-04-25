-- CPUステートとレイアウトをJSON化
module Serialize
  ( cpuStateJSON
  , layoutJSON
  , wireValsJSON
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Array (Array, elems, bounds)
import Data.Word (Word8)
import Data.Bits (shiftR, (.&.))
import Numeric (showHex)

import Circuit (WireVals)
import CPU.Types (CPUState(..), CPURefs(..))
import CPU.Components
import SimpleJSON

-- ──────────────────────────────────────────────
-- CPUステート
-- ──────────────────────────────────────────────
cpuStateJSON :: CPUState -> String
cpuStateJSON st = renderJSON $ jObj
  [ ("regA",      jStr (hex8 (csRegA st)))
  , ("regB",      jStr (hex8 (csRegB st)))
  , ("pc",        jStr (hex8 (csPC st)))
  , ("irOpcode",  jStr (hex8 (csIROpcode st)))
  , ("irOperand", jStr (hex8 (csIROperand st)))
  , ("regAVal",   JInt (fromIntegral (csRegA st)))
  , ("regBVal",   JInt (fromIntegral (csRegB st)))
  , ("pcVal",     JInt (fromIntegral (csPC st)))
  , ("flagZ",     jBool (csFlagZ st))
  , ("flagC",     jBool (csFlagC st))
  , ("flagN",     jBool (csFlagN st))
  , ("halted",    jBool (csHalted st))
  , ("instrName", jStr (opcodeToName (csIROpcode st)))
  , ("memory",    jArr (map (JInt . fromIntegral) (elems (csMemory st))))
  ]

hex8 :: Word8 -> String
hex8 v = "0x" ++ (if length h < 2 then '0':h else h)
  where h = showHex v ""

opcodeToName :: Word8 -> String
opcodeToName op = case op of
  0x00->"NOP";  0x01->"LOAD_A"; 0x02->"LOAD_B"; 0x03->"LOAD_A_MEM"
  0x04->"STORE_A"; 0x05->"ADD"; 0x06->"SUB"; 0x07->"AND"
  0x08->"OR";   0x09->"XOR";  0x0A->"NOT"; 0x0B->"JMP"
  0x0C->"JZ";   0x0D->"JNZ"; 0x0E->"SHL"; 0x0F->"HLT"; _->"???"

-- ──────────────────────────────────────────────
-- ワイヤ値 (wireId, value) のフラット配列
-- ──────────────────────────────────────────────
wireValsJSON :: WireVals -> String
wireValsJSON vs = renderJSON $ jArr
  [ jObj [("id", JInt k), ("v", JInt (if b then 1 else 0))]
  | (k, b) <- IM.toAscList vs ]

-- ──────────────────────────────────────────────
-- レイアウト (初期化時に1度だけ送る)
-- ──────────────────────────────────────────────
layoutJSON :: LayoutComp -> String
layoutJSON = renderJSON . compToJ

compToJ :: LayoutComp -> JVal
compToJ lc = jObj
  [ ("id",       JInt (lcId lc))
  , ("type",     jStr (lcType lc))
  , ("name",     jStr (lcName lc))
  , ("label",    jStr (lcLabel lc))
  , ("x",        jNum (lcX lc))
  , ("y",        jNum (lcY lc))
  , ("w",        jNum (lcW lc))
  , ("h",        jNum (lcH lc))
  , ("color",    jStr (lcColor lc))
  , ("expandAt", jNum (lcExpandAt lc))
  , ("children", jArr (map compToJ (lcChildren lc)))
  , ("gates",    jArr (map gateToJ (lcGates lc)))
  , ("wireSegs", jArr (map wireSegsToJ (lcWireSegs lc)))
  , ("inputs",   jArr (map pinToJ (lcInputs lc)))
  , ("outputs",  jArr (map pinToJ (lcOutputs lc)))
  ]

gateToJ :: LayoutGate -> JVal
gateToJ g = jObj
  [ ("id",   JInt (lgId g))
  , ("type", jStr (lgType g))
  , ("name", jStr (lgName g))
  , ("x",    jNum (lgX g)), ("y", jNum (lgY g))
  , ("w",    jNum (lgW g)), ("h", jNum (lgH g))
  , ("ins",  jArr (map JInt (lgIns g)))
  , ("out",  JInt (lgOut g))
  ]

wireSegsToJ :: (Int, [LayoutSeg]) -> JVal
wireSegsToJ (wid, segs) = jObj
  [ ("wireId", JInt wid)
  , ("segs",   jArr (map segToJ segs))
  ]

segToJ :: LayoutSeg -> JVal
segToJ s = jObj
  [ ("x1", jNum (lsX1 s)), ("y1", jNum (lsY1 s))
  , ("x2", jNum (lsX2 s)), ("y2", jNum (lsY2 s))
  , ("owner", JInt (lsOwner s))
  ]

pinToJ :: (String, LayoutPin) -> JVal
pinToJ (name, p) = jObj
  [ ("name", jStr name)
  , ("wire", JInt (lpWire p))
  , ("x",    jNum (lpX p))
  , ("y",    jNum (lpY p))
  ]
