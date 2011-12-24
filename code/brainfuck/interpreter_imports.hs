import qualified Data.IntMap as M
import Data.Char(chr,ord)
import Data.Array

type State = (Int,M.IntMap Int)
maxsize = 30000
data Dir =  Up | Down
