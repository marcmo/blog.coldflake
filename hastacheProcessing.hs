{-# LANGUAGE DeriveDataTypeable #-}
import Text.Hastache 
import Text.Hastache.Context 
import qualified Data.ByteString.Lazy as BSL 
import System

main = do 
    [f] <- getArgs
    let config = defaultConfig {muTemplateFileDir = (Just "code")}
    res <- hastacheFile config f (mkStrContext context) 
    BSL.putStrLn res 
    where 
      context _ = MuNothing
