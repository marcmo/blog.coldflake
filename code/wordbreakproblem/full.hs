import Data.List(inits)
import qualified Data.Set as S
import Maybe(isJust)
import Control.Applicative((<$>))
import System(getArgs)

type Dictionary = S.Set String

breakWords ::  String -> Dictionary -> Maybe [String]
breakWords xs dict = splittUp xs [] where
  splittUp :: String -> [String] -> Maybe [String]
  splittUp [] rs = Just $ reverse rs -- done, give back result
  splittUp xs rs = safeHead $ filter isJust solutions where
    solutions = [splittUp rest (match:rs) | (match,rest) <- findMatches xs dict]

-- find all solutions
breakWords2 xs dict = splittUp xs []
  where splittUp [] rs = [Just $ reverse rs] -- done, give back result
        splittUp xs rs = filter isJust $ concat solutions
          where solutions = [splittUp rest (match:rs) | (match,rest) <- findMatches xs dict]

findMatches :: String -> Dictionary -> [(String,String)]
findMatches xs dict = [(x,drop (length x) xs) | x <- reverse $ inits xs, x `S.member` dict]
          
safeHead xs = if null xs then Nothing else head xs

withDictFile :: String -> FilePath -> IO ()
withDictFile s f = S.fromList . lines <$> readFile f >>= print . breakWords s

main = do
  [s] <- getArgs
  withDictFile s "/usr/share/dict/words"
  

