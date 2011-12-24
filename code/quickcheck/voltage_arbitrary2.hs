data SlowRisingCurve = SlowRising [VoltageListener] [VoltageEvent]
instance Arbitrary SlowRisingCurve where
  arbitrary = do
    vs <- eventList 350 (2,1) [100..150]
    let vss = incV 5000 vs ++ [VoltageChange 12000]
    listener <- arbitrary
    return $ SlowRising listener vss


eventList :: Int -> (Int,Int) -> [Int] -> Gen [VoltageEvent]
eventList n (freqA,freqB) range = do
    let deltaV = elements range
    let deltaVN = negate <$> elements range
    let nextEvent = frequency [(freqA,VoltageChange <$> deltaV),
                               (freqB,VoltageChange <$> deltaVN),
                               (freqA+freqB,return TimeoutEvent)]
    vectorOf n nextEvent

incV :: Int -> [VoltageEvent] -> [VoltageEvent]
incV start xs = reverse $ snd $ foldl' addEvent (start,[]) xs
  where addEvent (s,rs) (VoltageChange x) = (s+x,VoltageChange (s+x):rs)
        addEvent (s,rs) timeout = (s,timeout:rs)
