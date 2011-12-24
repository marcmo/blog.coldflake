:set -w -fwarn-unused-binds -fwarn-unused-imports
import Text.Regex
import qualified Data.List as L
import qualified Data.Set as S
import Data.List.Split
import Maybe(isJust)
import Control.Arrow ((&&&))
import Data.Function(on)
import System.Directory
import Control.Monad(filterM)

let script f = getContents >>= return . f
let (=~) inp pat = isJust $ matchRegex (mkRegex pat) inp 
let uniq xs = S.toList $ S.fromList xs
let eachLine f = unlines . map f . lines
:set prompt "List,Set,Split,Regex> "
