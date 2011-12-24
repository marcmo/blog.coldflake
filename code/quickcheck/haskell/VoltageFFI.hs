{-# LANGUAGE ForeignFunctionInterface #-}
module VoltageFFI where

import Foreign
import Foreign.C.Types
import System.IO
import VoltageTests
import Control.Applicative

normal_voltage=0 :: Int
under_voltage_pending=1 :: Int
under_voltage=2 :: Int
hysteresis_voltag=3 :: Int

foreign import ccall "ftest.h voltageEvent"
     c_voltageEvent :: CInt -> IO ()
foreign import ccall "ftest.h timeoutEvent"
     c_timeoutEvent :: IO ()
foreign import ccall "ftest.h listenerVoltageState"
     c_voltageState :: CInt -> IO CInt
foreign import ccall "ftest.h underVoltageLeftCount"
     c_underVoltageLeftCount :: CInt -> IO CInt
foreign import ccall "ftest.h underVoltageOccuredCount"
     c_underVoltageCount :: CInt -> IO CInt
foreign import ccall "ftest.h setupTestExecuter"
     c_setUp :: IO ()
foreign import ccall "ftest.h addListener"
     c_addListener :: CInt -> CInt -> CInt -> IO ()
foreign import ccall "ftest.h tearDownTestExecuter"
     c_tearDown :: IO ()

voltageChangeC :: Int -> IO ()
voltageChangeC x = c_voltageEvent (fromIntegral x) >> return ()

voltageStateC :: Int -> IO (Int,Int,Int)
voltageStateC n = do
  finalState <- fromIntegral <$> c_voltageState (fromIntegral n)
  underVoltageCount <- fromIntegral <$> c_underVoltageCount (fromIntegral n)
  underVoltageLeftCount <- fromIntegral <$> c_underVoltageLeftCount (fromIntegral n)
  return (finalState,underVoltageCount,underVoltageLeftCount)

setUpC :: IO ()
setUpC = c_setUp

addListenerC :: VoltageListener -> IO ()
addListenerC (VL (a,b,c)) = c_addListener (fromIntegral a) (fromIntegral b) (fromIntegral c)

