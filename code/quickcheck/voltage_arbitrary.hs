data VoltageListener = VL (Int,Int,Int) deriving (Eq,Ord,Show)
instance Arbitrary VoltageListener where
  arbitrary = do
    threshold <- choose (5000,10000) 
    debounce <- elements [0,100] 
    hyst <- elements [0,100..500] 
    return $ VL (threshold,debounce,hyst)
