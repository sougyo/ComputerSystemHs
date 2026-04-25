{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (CString, newCString, peekCString)
import Foreign.C.Types (CInt(..))
import Data.Array (listArray, (//))
import Data.Word (Word8)

import Circuit (Gate)
import Builder (topoSort)
import CPU.Components (buildCPU, CPUBuildResult(..))
import CPU.Types (CPUState(..), CPURefs(..), initialCPUState)
import CPU.Step (stepCPU, resetCPU, loadProgram)
import Assembler (assemble)
import Serialize (cpuStateJSON, layoutJSON, wireValsJSON)

-- ──────────────────────────────────────────────
-- 起動時に1度だけ構築 (純粋なグローバル値)
-- ──────────────────────────────────────────────
{-# NOINLINE theResult #-}
theResult :: CPUBuildResult
theResult = buildCPU

{-# NOINLINE theSortedGates #-}
theSortedGates :: [Gate]
theSortedGates = topoSort (cbrGates theResult)

{-# NOINLINE theRefs #-}
theRefs :: CPURefs
theRefs = cbrRefs theResult

{-# NOINLINE globalState #-}
globalState :: IORef CPUState
globalState = unsafePerformIO $ newIORef (initialCPUState theRefs)

-- ──────────────────────────────────────────────
-- WASM エクスポート
-- ──────────────────────────────────────────────

foreign export ccall "hs_get_layout" hsGetLayout :: IO CString
hsGetLayout = newCString (layoutJSON (cbrLayout theResult))

foreign export ccall "hs_get_state" hsGetState :: IO CString
hsGetState = do
  st <- readIORef globalState
  newCString (cpuStateJSON st)

foreign export ccall "hs_get_wires" hsGetWires :: IO CString
hsGetWires = do
  st <- readIORef globalState
  newCString (wireValsJSON (csWires st))

foreign export ccall "hs_step" hsStep :: IO ()
hsStep = modifyIORef' globalState (stepCPU theSortedGates theRefs)

foreign export ccall "hs_reset" hsReset :: IO ()
hsReset = modifyIORef' globalState (resetCPU theRefs)

foreign export ccall "hs_load_asm" hsLoadAsm :: CString -> IO ()
hsLoadAsm cstr = do
  src <- peekCString cstr
  let instrs = assemble src
  modifyIORef' globalState (loadProgram instrs theRefs)

-- ──────────────────────────────────────────────
-- ネイティブ実行 / テスト
-- ──────────────────────────────────────────────
main :: IO ()
main = do
  putStrLn "=== ComputerSystem Haskell Simulator ==="
  putStrLn $ "Total gates (sorted): " ++ show (length theSortedGates)

  -- サンプルプログラム実行: A=25, B=17, ADD, HLT → A=42
  let prog = assemble "LOAD_A 25\nLOAD_B 17\nADD\nHLT"
  writeIORef globalState
    (loadProgram prog theRefs (initialCPUState theRefs))

  let loop = do
        st <- readIORef globalState
        if csHalted st
          then return st
          else do
            modifyIORef' globalState (stepCPU theSortedGates theRefs)
            loop

  final <- loop
  putStrLn $ "RegA = " ++ show (csRegA final)
             ++ "  (expected 42)"
  putStrLn $ "FlagZ = " ++ show (csFlagZ final)
  putStrLn $ "Halted = " ++ show (csHalted final)
  putStrLn "Done."
