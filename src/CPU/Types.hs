{-# LANGUAGE StrictData #-}
module CPU.Types
  ( CPURefs(..)
  , DecoderRefs(..)
  , CPUState(..)
  , initialCPUState
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
  , crMemRead    :: !Int
  , crMemWrite   :: !Int
  } deriving Show

-- メモリ: 256バイトの配列
type Mem = Array Int Word8

data CPUState = CPUState
  { csRegA      :: !Word8
  , csRegB      :: !Word8
  , csPC        :: !Word8
  , csIROpcode  :: !Word8
  , csIROperand :: !Word8
  , csFlagZ     :: !Bool
  , csFlagC     :: !Bool
  , csFlagN     :: !Bool
  , csHalted    :: !Bool
  , csMemory    :: !Mem
  , csWires     :: !WireVals
  } deriving Show

initialCPUState :: CPURefs -> CPUState
initialCPUState _refs = CPUState
  { csRegA      = 0
  , csRegB      = 0
  , csPC        = 0
  , csIROpcode  = 0
  , csIROperand = 0
  , csFlagZ     = False
  , csFlagC     = False
  , csFlagN     = False
  , csHalted    = False
  , csMemory    = listArray (0, 255) (repeat 0)
  , csWires     = mempty
  }
