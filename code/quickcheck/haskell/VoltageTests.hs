module VoltageTests where
import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Control.Monad
import Control.Applicative
import Data.List(foldl',sort,scanl,repeat)
import Debug.Trace

data VoltageListener = VL (Int,Int,Int) deriving (Eq,Ord,Show)
instance Arbitrary VoltageListener where
  arbitrary = do
    threshold <- choose (5000,10000)
    debounce <- elements [0,100]
    hyst <- elements [0,100..500]
    return $ VL (threshold,debounce,hyst)

data VoltageEvent = VoltageChange Int
                    | TimeoutEvent
                    | Register VoltageListener
                    | Deregister VoltageListener deriving(Eq,Ord)
instance Show VoltageEvent where
  show (VoltageChange x) = show x
  show TimeoutEvent = "T"
instance Arbitrary VoltageEvent where
  arbitrary = frequency [(2,VoltageChange <$> choose(1,16000)),
                         (1,return TimeoutEvent)]

eventList :: Int -> (Int,Int) -> [Int] -> Gen [VoltageEvent]
eventList n (freqA,freqB) range = do
    let nextEvent = frequency [(freqA,VoltageChange <$> elements range),
                               (freqB,VoltageChange <$> negate <$> elements range),
                               (freqA+freqB,return TimeoutEvent)]
    vectorOf n nextEvent

incV :: Int -> [VoltageEvent] -> [VoltageEvent]
incV start xs = reverse $ snd $ foldl' addEvent (start,[]) xs
  where addEvent (s,rs) (VoltageChange x) = (s+x,VoltageChange (s+x):rs)
        addEvent (s,rs) timeout = (s,timeout:rs)

data SlowRisingCurve = SlowRising [VoltageListener] [VoltageEvent] deriving (Show)
instance Arbitrary SlowRisingCurve where
  arbitrary = do
    vs <- eventList 350 (2,1) [100..150]
    let vss = incV 5000 vs ++ [VoltageChange 12000]
    listener <- arbitrary
    return $ SlowRising listener vss

data ErraticCurve = Erratic VoltageListener [VoltageEvent] deriving (Show)
instance Arbitrary ErraticCurve where
  arbitrary = do
    vs <- vectorOf 150 arbitrary
    listener <- arbitrary
    return $ Erratic listener (vs ++ [VoltageChange 12000])

data RiseFallRiseCurve = RiseFallRise VoltageListener [VoltageEvent] deriving (Show)
instance Arbitrary RiseFallRiseCurve where
  arbitrary = do
    vs <- eventList 130 (2,1) [100..150]
    vs2 <- eventList 100 (1,2) [100..150]-- declining
    vs3 <- eventList 170 (2,1) [100..150]
    let vss = incV 5000 (vs++vs2++vs3) ++ [VoltageChange 12000]
    listener <- arbitrary
    return $ RiseFallRise listener vss

data AbruptDropsCurve = AbruptDrops VoltageListener [VoltageEvent] deriving (Show)
instance Arbitrary AbruptDropsCurve where
  arbitrary = do
    let change = frequency [
                    (10, VoltageChange <$> choose (-50,50)),
                    ( 1, VoltageChange <$> choose (-11000,0))]
    cp <- vectorOf 200 change
    let vs = foldl' (\acc (VoltageChange x)-> VoltageChange x:VoltageChange (-x):acc) [] cp
    let vss = incV 12000 vs
    listener <- arbitrary
    return $ AbruptDrops listener vss

data VoltageCurve4 = VC4 VoltageListener [VoltageEvent] deriving (Show)
instance Arbitrary VoltageCurve4 where
  arbitrary = do
    vs <- eventList 130 (2,1) [1..200]
    vs2 <- eventList 100 (1,2) [1..200]-- declining
    vs3 <- eventList 170 (2,1) [1..200]
    let vss = incV 5000 (vs++vs2++vs3) ++ [VoltageChange 12000]
    listener <- arbitrary
    return $ VC4 listener vss

data VoltageCurve5 = VC5 VoltageListener [VoltageEvent] deriving (Show)
instance Arbitrary VoltageCurve5 where
  arbitrary = do
    vs <- eventList 5000 (2,1) [1..10]
    let hyst = 100
    let vss' = incV 1000 vs
    let vss = takeWhile (voltageBelow (2000+hyst+1)) vss' ++ [VoltageChange (2000+hyst+1)]
    let listener = VL (2000,0,hyst)
    return $ VC5 listener vss
  shrink e@(VC5 x (a:b:[])) = []
  shrink (VC5 x xs) = map (VC5 x) (shrinkList shrink xs)

voltageBelow x (VoltageChange y) = y < x
voltageBelow _ TimeoutEvent = True

displayVC5 = do
  ss <- sample' arbitrary :: IO [VoltageCurve5]
  mapM_ printInfo ss

isVoltageChange (VoltageChange _) = True
isVoltageChange _ = False

getVolt (VoltageChange x) = x

printInfo (VC5 _ vs) = do
  let fs = filter isVoltageChange vs
  let fss = map getVolt fs
  print fss

testI = do
  vs <- eventList 100 (2,1) [1..10]
  let hyst = 100
  let vss' = incV 2050 vs
  let vss = takeWhile (voltageBelow (2000+hyst+1)) vss' ++ [VoltageChange (2000+hyst+1)]
  return (vss',vss)

displayI = sample' testI >>= mapM_ printInfo2

printInfo2 (vs',vs) = do
  let fs = filter isVoltageChange vs
  let fss = map getVolt fs
  let fs' = filter isVoltageChange vs'
  let fss' = map getVolt fs'
  putStrLn $ "vs':" ++ show fss'
  putStrLn $ "vs:" ++ show fss
