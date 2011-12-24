{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Plots where
import VoltageTests
import Test.QuickCheck
import Data.Aeson
import Control.Applicative((<*>))
import qualified Data.Aeson.Types as T
import qualified Data.Vector as V

import Data.Attoparsec (parse, IResult(..))
import Control.Applicative ((<$>))
import Control.Monad (mzero)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL

plotPng :: Int -> String -> IO ()
plotPng maxC n = do
  header <- readFile "gruff.tmpl" 
  contents <- readFile ("out/" ++ n ++ ".txt") 
  let content = unlines $ take maxC (lines contents)
  writeFile ("out/" ++ n ++ ".rb") $
    header ++ content ++ footer
      where footer = "g.write('voltage" ++ n ++ ".png')"

data FileMode = Append | New
logData :: Record -> FileMode -> IO ()
logData r@(Record p (PointData pd)) Append = do
    let f = "out/" ++ p ++ ".json"
    content <- BS.readFile f
    let parsed = case parse json content of
                    (Done rest r) -> T.parseMaybe parseJSON r :: Maybe Record
                    _             -> Nothing
    maybe (print "parse unsuccessfull") 
          (\(Record n (PointData xs))-> do
              let res = Record n (PointData (pd ++ xs))
              BSL.writeFile f (BSL.append (encode res) "\n"))
          parsed
logData r@(Record p _) New = BSL.writeFile ("out/" ++ p ++ ".json") (BSL.append (encode r) "\n")

createCurves ::  IO ()
createCurves = do
  curves <- sample' arbitrary :: IO [SlowRisingCurve]
  let eventSets = [eventsA | (SlowRising _ eventsA) <- curves]
  curves2 <- sample' arbitrary :: IO [ErraticCurve]
  let eventSets2 = [eventsA | (Erratic _ eventsA) <- curves2]
  curves3 <- sample' arbitrary :: IO [RiseFallRiseCurve]
  let eventSets3 = [eventsA | (RiseFallRise _ eventsA) <- curves3]
  curves4 <- sample' arbitrary :: IO [VoltageCurve4]
  let eventSets4 = [eventsA | (VC4 _ eventsA) <- curves4]
  -- curves5 <- sample' arbitrary :: IO [VoltageCurve5]
  -- let eventSets5 = [eventsA | (VC5 _ eventsA) <- curves5]
  curves5 <- sample' arbitrary :: IO [AbruptDropsCurve]
  let eventSets5 = [eventsA | (AbruptDrops _ eventsA) <- curves5]
  createLog 5 "A" eventSets New
  createLog 5 "B" eventSets2 New
  createLog 5 "C" eventSets3 New
  createLog 5 "D" eventSets4 New
  createLog 1 "E" eventSets5 New

createLog ::  Int -> String -> [[VoltageEvent]] -> FileMode -> IO ()
createLog n name eventSetsToLog a = do
  let voltageChangeSets = take n [map getVolt (filter isVoltageChange eventSet)| eventSet <- eventSetsToLog] :: [[Int]]
  let pointLists = map createPointList voltageChangeSets
  let r = Record name $ PointData pointLists
  logData r a
    where createPointList vs = PointList [Point (a,b)| (a,b) <- zip [1..] vs]

data Event = Event Int Int | EventLog [Event] deriving (Show)

instance FromJSON Event where
  parseJSON (Object v) = Event <$> v .: "x" <*> v .: "y"
  parseJSON (Array a)    = EventLog <$> mapM parseJSON (V.toList a)
  parseJSON _ = mzero

instance ToJSON Event where
  toJSON (Event s n) = object [ "x" .= s, "y" .= n ]

parseMsgFromString bs =
  case parse json bs of
       (Done rest r) -> T.parseMaybe parseJSON r :: Maybe Event
       _             -> Nothing

exampleJSONMessage = "[{\"x\":33, \"y\":555},{\"x\":34, \"y\":55}]"

testMain2 = do
  print $ parseMsgFromString exampleJSONMessage
  let reply = Event 222 25
  putStrLn $ "Encoded reply: " ++ BSL.unpack (encode reply)

testMain = do
  let record = Record "testme" (PointData [PointList [Point (2,2)], PointList [Point (1,1),Point (3,3)]])
  putStrLn $ "Encoded back: " ++ BSL.unpack (encode record)
  let parsed = case parse json sampleRecord of
                  (Done rest r) -> T.parseMaybe parseJSON r :: Maybe Record
                  _             -> Nothing
  print parsed
  

data Point = Point (Int,Int) deriving (Show)
data PointList = PointList [Point] deriving (Show)
data PointData = PointData [PointList] deriving (Show)
data Record = Record String PointData deriving (Show)

instance FromJSON Point where
    parseJSON (Array a) = toPair a
      where toPair ::  V.Vector Value -> T.Parser Point
            toPair v = Point <$> ((,) <$> parseJSON a <*> parseJSON b)
                where [a,b] = V.toList v
instance FromJSON PointList where
    parseJSON (Array a) = PointList <$> mapM parseJSON (V.toList a)
instance FromJSON PointData where
    parseJSON (Array a) = PointData <$> mapM parseJSON (V.toList a)
instance FromJSON Record where
    parseJSON (Object v) = Record <$> v .: "label" <*> v.: "data"

instance ToJSON Record where
  toJSON (Record label pd) = object [ "label" .= toJSON label, "data" .= toJSON pd]
instance ToJSON Point where
  toJSON (Point (a,b)) = Array $ V.fromList [toJSON x | x <- [a,b]]
instance ToJSON PointList where
  toJSON (PointList ps) = Array $ V.fromList [toJSON x | x <- ps]
instance ToJSON PointData where
  toJSON (PointData ps) = Array $ V.fromList [toJSON x | x <- ps]

sampleRecord = "{ \"label\": \"testlabel\", \"data\": [[[1999, 3.0], [2000, 3.9], [2001, 2.0], [2002, 1.2], [2003, 1.3], [2004, 2.5], [2005, 2.0], [2006, 3.1], [2007, 2.9], [2008, 0.9]],[[1999, -0.1], [2000, 2.9], [2001, 0.2], [2002, 0.3], [2003, 1.4], [2004, 2.7], [2005, 1.9], [2006, 2.0], [2007, 2.3], [2008, -0.7]]] }"
