{-# LANGUAGE StrictData #-}
module CPU.Types
  ( CPURefs(..)
  , DecoderRefs(..)
  , CPUState(..)
  , initialCPUState
  , displayBase
  , displaySize
  , kbDataAddr
  , irqVectorAddr
  , memTotal
  ) where

import Data.Word (Word8)
import Data.Array (Array, listArray)

import Circuit (WireVals)

data DecoderRefs = DecoderRefs
  { drRegWrite :: !Int
  , drMemRead  :: !Int
  , drMemWrite :: !Int
  , drAluOp0   :: !Int
  , drAluOp1   :: !Int
  , drSubMode  :: !Int
  , drBranch   :: !Int
  , drBranchZ  :: !Int
  , drHalt     :: !Int
  , drLoadImm  :: !Int
  , drRegDstB  :: !Int
  } deriving Show

-- ディスプレイ MMIO 定数 (5×5 ピクセル)
displayBase :: Int
displayBase = 0xE0   -- 先頭アドレス (pixel(0,0))

displaySize :: Int
displaySize = 25     -- 5×5 = 25 ピクセル

-- キーボード / 割り込み MMIO 定数
kbDataAddr :: Int
kbDataAddr = 0xDC    -- 最後に押されたキーの ASCII コード

irqVectorAddr :: Int
irqVectorAddr = 0xFE -- 割り込みハンドラの開始アドレス (1 バイト)

data CPURefs = CPURefs
  { crRegAIns    :: ![Int]
  , crRegAOuts   :: ![Int]
  , crRegBIns    :: ![Int]
  , crRegBOuts   :: ![Int]
  , crPCOuts     :: ![Int]
  , crIRIns      :: ![Int]
  , crIROuts     :: ![Int]
  , crIRLIns     :: ![Int]
  , crIRLOuts    :: ![Int]
  , crDecOpIns   :: ![Int]
  , crDecOuts    :: !DecoderRefs
  , crALUAIns    :: ![Int]
  , crALUBIns    :: ![Int]
  , crALUOp0     :: !Int
  , crALUOp1     :: !Int
  , crALUSub     :: !Int
  , crALUResults :: ![Int]
  , crALUZero    :: !Int
  , crALUCout    :: !Int
  , crMemAddrIns :: ![Int]
  , crMemDatOuts :: ![Int]
  , crMemRead       :: !Int
  , crMemWrite      :: !Int
  , crDisplayPixels :: ![Int]   -- 25本のピクセルワイヤ (アドレス 0xE0..0xF8 に対応)
  } deriving Show

-- メモリ: 512バイト (2バンク × 256バイト)
-- LOAD_A_MEM / STORE_A の operand は 8bit のため、上位 9bit 目は csBank で決まる。
-- bank=0 (低位 256B): コード、スタック、display、MMIO
-- bank=1 (高位 256B): プログラム任意のデータ領域
memTotal :: Int
memTotal = 512

type Mem = Array Int Word8

data CPUState = CPUState
  { csRegA       :: !Word8
  , csRegB       :: !Word8
  , csPC         :: !Word8
  , csSP         :: !Word8
  , csIROpcode   :: !Word8
  , csIROperand  :: !Word8
  , csFlagZ      :: !Bool
  , csFlagC      :: !Bool
  , csFlagN      :: !Bool
  , csHalted     :: !Bool
  , csMemory     :: !Mem
  , csWires      :: !WireVals
  , csIRQPending :: !Bool
  , csIRQEnabled :: !Bool
  } deriving Show

initialCPUState :: CPURefs -> CPUState
initialCPUState _refs = CPUState
  { csRegA       = 0
  , csRegB       = 0
  , csPC         = 0
  -- スタックは display (0xE0..0xF8) と IRQ ベクタ (0xFE) を避けて
  -- mem[0xFC] から下方向に伸ばす。csMemory の上位バンク (0x100..) は
  -- stack インデックス (Word8) からは届かないため、 stack は常に bank 0。
  , csSP         = 0xFD
  , csIROpcode   = 0
  , csIROperand  = 0
  , csFlagZ      = False
  , csFlagC      = False
  , csFlagN      = False
  , csHalted     = False
  , csMemory     = listArray (0, memTotal - 1) (repeat 0)
  , csWires      = mempty
  , csIRQPending = False
  , csIRQEnabled = False
  }
