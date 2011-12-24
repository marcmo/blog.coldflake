{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import VoltageFFI
import VoltageTests
import Test.QuickCheck
import Test.QuickCheck.Monadic(monadicIO,run,assert)
import Plots
import Control.Applicative
import Control.Monad(when,forM_)

main = do
  let x = stdArgs { maxSuccess = 500 }
  quickCheckWith x prop_neverMissLeftUndervoltage
  quickCheckWith x prop_neverMissLeftUndervoltage2
  quickCheckWith x prop_neverMissLeftUndervoltage3
  quickCheckWith x prop_neverMissLeftUndervoltage4
  quickCheckWith (stdArgs { maxSuccess = 100 }) prop_neverMissLeftUndervoltage5
  quickCheckWith x prop_abruptDropsDoNotCauseUnderVoltage

prop_neverMissLeftUndervoltage (SlowRising listeners events) = 
  length listeners > 0 ==> monadicIO $ do
    run $ setUpC
    run $ forM_ listeners $ \x -> addListenerC x
    run $ mapM sendEvent events
    forM_ [0..length listeners - 1] $ \n -> do
      (finalState,enteredCount,leftCount) <- run $ voltageStateC n
      assert $ finalState == normal_voltage
      assert $ enteredCount == leftCount
    run c_tearDown

prop_neverMissLeftUndervoltage2 (Erratic (VL (t,d,h)) events) = monadicIO $
  runTestsForCurve (t,d,h) events "B"
prop_neverMissLeftUndervoltage3 (RiseFallRise (VL (t,d,h)) events) = monadicIO $
  runTestsForCurve (t,d,h) events "C"
prop_neverMissLeftUndervoltage4 (VC4 (VL (t,d,h)) events) = monadicIO $
  runTestsForCurve (t,d,h) events "D"
prop_neverMissLeftUndervoltage5 (VC5 (VL (t,d,h)) events) = monadicIO $
  runTestsForCurve (t,d,h) (events ++ [VoltageChange 3000]) "E"
prop_abruptDropsDoNotCauseUnderVoltage (AbruptDrops listener@(VL (t,debounceTime,h)) events) =
  (debounceTime >= 5) && (lastVoltage events > (t+h)) ==> monadicIO $ do
      run $ setUpC
      run $ addListenerC listener
      run $ mapM sendEvent events
      (finalState,enteredCount,leftCount) <- run $ voltageStateC 0
      run c_tearDown
      assert $ finalState == normal_voltage
      assert $ enteredCount == 0
  
  

sendEvent :: VoltageEvent -> IO ()
sendEvent (VoltageChange x) = voltageChangeC x
sendEvent (TimeoutEvent) = c_timeoutEvent

runTestsForCurve (t,d,h) events n = do
    run $ setUpC
    run $ addListenerC (VL (t,d,h))
    run $ mapM sendEvent events
    -- run $ createLog 5 n [events] Append
    (finalState,enteredCount,leftCount) <- run $ voltageStateC 0
    run c_tearDown
    when (lastVoltage events > (t+h)) $ do
        (assert $ finalState == normal_voltage)
        (assert $ enteredCount == leftCount)

lastVoltage :: [VoltageEvent] -> Int
lastVoltage events = pick (reverse events)
  where pick ((VoltageChange y):xs) = y
        pick (TimeoutEvent:xs) = pick xs


