{-# LANGUAGE StrictData #-}
module CPU.Step (stepCPU, resetCPU, loadProgram) where

import Data.Bits (shiftR, shiftL, (.&.), complement)
import Data.Word (Word8)
import Data.Array (Array, (!), (//), listArray, elems)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')

import Circuit
import CPU.Types

-- ──────────────────────────────────────────────
-- ワイヤ操作ヘルパー
-- ──────────────────────────────────────────────

driveWord8 :: [Int] -> Word8 -> WireVals -> WireVals
driveWord8 wids val vs =
  foldl' (\acc (wid, bit) ->
    IM.insert wid (((fromIntegral val `shiftR` bit) .&. (1::Int)) == 1) acc)
    vs (zip wids [0..7])

readWord8 :: [Int] -> WireVals -> Word8
readWord8 wids vs =
  fromIntegral $ sum
    [ if IM.findWithDefault False wid vs then (1::Int) `shiftL` i else 0
    | (wid, i) <- zip wids [0..7] ]

w8bool :: Int -> WireVals -> Bool
w8bool wid vs = IM.findWithDefault False wid vs

-- ──────────────────────────────────────────────
-- ALU制御信号
-- ──────────────────────────────────────────────
setALUControl :: CPURefs -> Word8 -> WireVals -> WireVals
setALUControl refs opcode vs =
  let (op0, op1, sub) = aluCtrl opcode
  in IM.insert (crALUOp0 refs) op0
   $ IM.insert (crALUOp1 refs) op1
   $ IM.insert (crALUSub  refs) sub vs
  where
    aluCtrl 0x05 = (False, False, False)  -- ADD
    aluCtrl 0x06 = (False, False, True)   -- SUB
    aluCtrl 0x07 = (True,  False, False)  -- AND
    aluCtrl 0x08 = (False, True,  False)  -- OR
    aluCtrl 0x09 = (True,  True,  False)  -- XOR
    aluCtrl _    = (False, False, False)

-- ──────────────────────────────────────────────
-- CPUの1ステップ (純粋関数)
-- ──────────────────────────────────────────────
stepCPU :: [Gate] -> CPURefs -> CPUState -> CPUState
stepCPU gates refs st
  | csHalted st = st
  | otherwise   = st'
  where
    mem    = csMemory st
    pc     = fromIntegral (csPC st) :: Int
    opcode = mem ! pc
    opernd = mem ! (pc + 1)

    -- FETCH + デコーダ評価
    vs1 = evaluate gates
        $ driveWord8 (crDecOpIns refs) opcode
        $ driveWord8 (crIRIns refs) opcode
        $ driveWord8 (crIRLIns refs) opernd (csWires st)

    -- ALU入力セット + 評価
    vs2 = evaluate gates
        $ driveWord8 (crALUAIns refs) (csRegA st)
        $ driveWord8 (crALUBIns refs) (csRegB st)
        $ setALUControl refs opcode vs1

    aluResult = readWord8 (crALUResults refs) vs2
    aluCout   = w8bool (crALUCout refs) vs2

    -- 命令実行 (純粋)
    (newRegA, newRegB, newPC, newMem, newFlagC, writeA) =
      execInstr opcode opernd (csRegA st) (csRegB st)
                (fromIntegral (csPC st)) mem aluResult aluCout

    newFlagZ = writeA && (newRegA == 0)
    newFlagN = writeA && ((newRegA .&. 0x80) /= 0)
    newHalted = opcode == 0x0F

    -- レジスタ出力ワイヤ更新 (レンダリング用)
    vs3 = driveWord8 (crRegAOuts refs) newRegA
        $ driveWord8 (crRegBOuts refs) newRegB
        $ driveWord8 (crPCOuts   refs) (fromIntegral newPC)
        $ driveWord8 (crIROuts   refs) opcode
        $ driveWord8 (crIRLOuts  refs) opernd vs2

    st' = st
      { csRegA      = newRegA
      , csRegB      = newRegB
      , csPC        = fromIntegral (newPC .&. 0xFF)
      , csIROpcode  = opcode
      , csIROperand = opernd
      , csFlagZ     = newFlagZ
      , csFlagC     = newFlagC
      , csFlagN     = newFlagN
      , csHalted    = newHalted
      , csMemory    = newMem
      , csWires     = vs3
      }

-- 命令実行の純粋部分
execInstr :: Word8 -> Word8
          -> Word8 -> Word8
          -> Int
          -> Array Int Word8
          -> Word8 -> Bool
          -> (Word8, Word8, Int, Array Int Word8, Bool, Bool)
execInstr op opr regA regB pc mem aluRes aluCout =
  let nextPC = pc + 2
  in case op of
    0x00 -> (regA, regB, nextPC, mem, False, False)
    0x01 -> (opr,  regB, nextPC, mem, False, True)
    0x02 -> (regA, opr,  nextPC, mem, False, False)
    0x03 -> let a = mem ! fromIntegral opr
            in (a, regB, nextPC, mem, False, True)
    0x04 -> let m = mem // [(fromIntegral opr, regA)]
            in (regA, regB, nextPC, m, False, False)
    0x05 -> (aluRes, regB, nextPC, mem, aluCout, True)
    0x06 -> (aluRes, regB, nextPC, mem, not aluCout, True)
    0x07 -> (aluRes, regB, nextPC, mem, False, True)
    0x08 -> (aluRes, regB, nextPC, mem, False, True)
    0x09 -> (aluRes, regB, nextPC, mem, False, True)
    0x0A -> let r = (complement regA) .&. 0xFF
            in (r, regB, nextPC, mem, False, True)
    0x0B -> (regA, regB, fromIntegral opr, mem, False, False)
    0x0C -> let t = if regA == 0 then fromIntegral opr else nextPC
            in (regA, regB, t, mem, False, False)
    0x0D -> let t = if regA /= 0 then fromIntegral opr else nextPC
            in (regA, regB, t, mem, False, False)
    0x0E -> let c = (regA .&. 0x80) /= 0
                r = (regA `shiftL` 1) .&. 0xFF
            in (r, regB, nextPC, mem, c, True)
    _    -> (regA, regB, nextPC, mem, False, False)

-- ──────────────────────────────────────────────
resetCPU :: CPURefs -> CPUState -> CPUState
resetCPU refs st = (initialCPUState refs) { csMemory = csMemory st }

loadProgram :: [(Word8, Word8)] -> CPURefs -> CPUState -> CPUState
loadProgram instrs refs st =
  let updates = concatMap (\(i, (op, arg)) -> [(i*2, op), (i*2+1, arg)])
                  (zip [0..127] instrs)
      newMem  = listArray (0,255) (repeat 0) // updates
  in (resetCPU refs st) { csMemory = newMem }
