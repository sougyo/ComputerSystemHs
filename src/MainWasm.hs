{-# LANGUAGE ForeignFunctionInterface #-}
-- WASM リアクターモード用エントリ。
-- -no-hs-main でコンパイルすることで _initialize が生成され、
-- proc_exit が呼ばれず RTS が生き続ける。
module Main where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Foreign.C.String (CString, newCString, peekCString)

import Circuit (Gate)
import Builder (topoSort)
import CPU.Components (buildCPU, CPUBuildResult(..))
import CPU.Types (CPUState(..), CPURefs(..), initialCPUState)
import CPU.Step (stepCPU, resetCPU, loadProgram)
import Assembler (assemble)
import Serialize (cpuStateJSON, layoutJSON, wireValsJSON)

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

-- -no-hs-main 使用時も GHC は Main モジュールの main を要求する
main :: IO ()
main = return ()
