{-# LANGUAGE StrictData #-}
module CPU.Step (stepCPU, resetCPU, loadProgram) where

import Data.Bits (shiftR, shiftL, (.&.), complement)
import Data.Word (Word8)
import Data.Array (Array, (!), (//), listArray, elems)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')

import Circuit
import CPU.Types (CPURefs(..), CPUState(..), initialCPUState, displayBase, irqVectorAddr, memTotal)

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
-- 割り込み発火 (IRQ pending && enabled)
-- ──────────────────────────────────────────────
fireIRQ :: CPUState -> CPUState
fireIRQ st =
  let sp'    = csSP st - 1
      vec    = fromIntegral (csMemory st ! irqVectorAddr)
      newMem = csMemory st // [(fromIntegral sp', csPC st)]
  in st { csSP         = sp'
        , csPC         = vec
        , csMemory     = newMem
        , csIRQPending = False
        , csIRQEnabled = False
        }

-- ──────────────────────────────────────────────
-- CPUの1ステップ (純粋関数)
-- ──────────────────────────────────────────────
stepCPU :: [Gate] -> CPURefs -> CPUState -> CPUState
stepCPU gates refs st
  | csHalted st                          = st
  | csIRQPending st && csIRQEnabled st   = fireIRQ st
  | otherwise                            = st''
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
    (newRegA, newRegB, newSP, newPC, newMem, newFlagC, writeA) =
      execInstr opcode opernd (csRegA st) (csRegB st) (csSP st)
                (fromIntegral (csPC st)) mem aluResult aluCout

    -- CMP (0x14): A を変更せず ALU 結果でフラグ更新
    newFlagZ
      | opcode == 0x14 = aluResult == 0
      | otherwise      = writeA && (newRegA == 0)
    newFlagN
      | opcode == 0x14 = (aluResult .&. 0x80) /= 0
      | otherwise      = writeA && ((newRegA .&. 0x80) /= 0)
    newHalted = opcode == 0x0F

    -- レジスタ出力ワイヤ更新 (レンダリング用)
    vs3 = driveWord8 (crRegAOuts refs) newRegA
        $ driveWord8 (crRegBOuts refs) newRegB
        $ driveWord8 (crPCOuts   refs) (fromIntegral newPC)
        $ driveWord8 (crIROuts   refs) opcode
        $ driveWord8 (crIRLOuts  refs) opernd vs2

    -- ディスプレイピクセルワイヤ更新: MMIO アドレス 0xE0..0xF8 をワイヤに反映
    vs4 = foldl' (\acc (wid, addr) ->
                    IM.insert wid (newMem ! addr /= 0) acc)
                  vs3 (zip (crDisplayPixels refs) [displayBase..])

    st' = st
      { csRegA      = newRegA
      , csRegB      = newRegB
      , csPC        = fromIntegral (newPC .&. 0xFF)
      , csSP        = newSP
      , csIROpcode  = opcode
      , csIROperand = opernd
      , csFlagZ     = newFlagZ
      , csFlagC     = newFlagC
      , csFlagN     = newFlagN
      , csHalted    = newHalted
      , csMemory    = newMem
      , csWires     = vs4
      }

    -- RTI/EI/DI: 制御フラグ更新
    st'' = case opcode of
      0x1A -> st' { csIRQEnabled = True  }              -- RTI: re-enable interrupts
      0x1B -> st' { csIRQEnabled = True  }              -- EI
      0x1C -> st' { csIRQEnabled = False }              -- DI
      _    -> st'

-- 命令実行の純粋部分
execInstr :: Word8 -> Word8
          -> Word8 -> Word8 -> Word8
          -> Int
          -> Array Int Word8
          -> Word8 -> Bool
          -> (Word8, Word8, Word8, Int, Array Int Word8, Bool, Bool)
execInstr op opr regA regB sp pc mem aluRes aluCout =
  let nextPC  = pc + 2
      addrLo  = fromIntegral opr           -- bank 0 (0x000..0x0FF)
      addrHi  = 0x100 + fromIntegral opr   -- bank 1 (0x100..0x1FF)
  in case op of
    0x00 -> (regA, regB, sp, nextPC, mem, False, False)
    0x01 -> (opr,  regB, sp, nextPC, mem, False, True)
    0x02 -> (regA, opr,  sp, nextPC, mem, False, False)
    0x03 -> let a = mem ! addrLo
            in (a, regB, sp, nextPC, mem, False, True)
    0x04 -> let m = mem // [(addrLo, regA)]
            in (regA, regB, sp, nextPC, m, False, False)
    0x05 -> (aluRes, regB, sp, nextPC, mem, aluCout, True)
    0x06 -> (aluRes, regB, sp, nextPC, mem, not aluCout, True)
    0x07 -> (aluRes, regB, sp, nextPC, mem, False, True)
    0x08 -> (aluRes, regB, sp, nextPC, mem, False, True)
    0x09 -> (aluRes, regB, sp, nextPC, mem, False, True)
    0x0A -> let r = (complement regA) .&. 0xFF
            in (r, regB, sp, nextPC, mem, False, True)
    0x0B -> (regA, regB, sp, fromIntegral opr, mem, False, False)
    0x0C -> let t = if regA == 0 then fromIntegral opr else nextPC
            in (regA, regB, sp, t, mem, False, False)
    0x0D -> let t = if regA /= 0 then fromIntegral opr else nextPC
            in (regA, regB, sp, t, mem, False, False)
    0x0E -> let c = (regA .&. 0x80) /= 0
                r = (regA `shiftL` 1) .&. 0xFF
            in (r, regB, sp, nextPC, mem, c, True)
    -- 新命令 (0x10〜0x19)
    0x10 -> let a = mem ! addrLo                     -- LOAD_B_MEM: B ← mem[addr]
            in (regA, a, sp, nextPC, mem, False, False)
    0x11 -> (regA, regA, sp, nextPC, mem, False, False) -- MOV_B_A: B ← A
    0x12 -> let r = (fromIntegral regA * fromIntegral regB) .&. (0xFF :: Int) -- MUL
                c = (fromIntegral regA * fromIntegral regB) > (0xFF :: Int)
            in (fromIntegral r, regB, sp, nextPC, mem, c, True)
    0x13 -> let c = (regA .&. 0x01) /= 0            -- SHR: A ← A >> 1
                r = regA `shiftR` 1
            in (r, regB, sp, nextPC, mem, c, True)
    0x14 -> (regA, regB, sp, nextPC, mem, False, False) -- CMP: フラグはstepCPU側で処理
    0x15 -> let sp' = sp - 1                         -- PUSH: mem[sp-1] ← A; SP ← SP-1
                m   = mem // [(fromIntegral sp', regA)]
            in (regA, regB, sp', nextPC, m, False, False)
    0x16 -> let a   = mem ! fromIntegral sp           -- POP: A ← mem[SP]; SP ← SP+1
                sp' = sp + 1
            in (a, regB, sp', nextPC, mem, False, True)
    0x17 -> let sp' = sp - 1                         -- CALL: push(PC+2); JMP addr
                m   = mem // [(fromIntegral sp', fromIntegral nextPC)]
            in (regA, regB, sp', fromIntegral opr, m, False, False)
    0x18 -> let pc' = fromIntegral (mem ! fromIntegral sp)   -- RET: PC ← mem[SP]; SP ← SP+1
                sp' = sp + 1
            in (regA, regB, sp', pc', mem, False, False)
    0x19 -> let flagN = (regA .&. 0x80) /= 0         -- JNS: N=0 なら JMP addr
                t = if not flagN then fromIntegral opr else nextPC
            in (regA, regB, sp, t, mem, False, False)
    0x1A -> let pc' = fromIntegral (mem ! fromIntegral sp)   -- RTI: PC ← mem[SP]; SP ← SP+1
                sp' = sp + 1
            in (regA, regB, sp', pc', mem, False, False)
    0x1B -> (regA, regB, sp, nextPC, mem, False, False) -- EI: enable irq (handled in stepCPU)
    0x1C -> (regA, regB, sp, nextPC, mem, False, False) -- DI: disable irq (handled in stepCPU)
    0x1D -> let a = mem ! addrHi                       -- LOAD_A_HIGH: A ← mem[0x100+addr]
            in (a, regB, sp, nextPC, mem, False, True)
    0x1E -> let m = mem // [(addrHi, regA)]            -- STORE_A_HIGH: mem[0x100+addr] ← A
            in (regA, regB, sp, nextPC, m, False, False)
    0x1F -> let a = mem ! addrHi                       -- LOAD_B_HIGH: B ← mem[0x100+addr]
            in (regA, a, sp, nextPC, mem, False, False)
    _    -> (regA, regB, sp, nextPC, mem, False, False)

-- ──────────────────────────────────────────────
resetCPU :: CPURefs -> CPUState -> CPUState
resetCPU refs st = (initialCPUState refs) { csMemory = csMemory st }

loadProgram :: [(Word8, Word8)] -> CPURefs -> CPUState -> CPUState
loadProgram instrs refs st =
  let updates = concatMap (\(i, (op, arg)) -> [(i*2, op), (i*2+1, arg)])
                  (zip [0..127] instrs)
      newMem  = listArray (0, memTotal - 1) (repeat 0) // updates
  in (resetCPU refs st) { csMemory = newMem }
