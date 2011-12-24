import Data.List.Zipper
import Examples

run :: String -> IO String
run = fmap toList . flip step (fromList $ replicate 30000 '\NUL') . fromList

type Program = Zipper Char
type DataArea = Zipper Char

step :: Program -> DataArea -> IO (DataArea)
step prog s = if endp prog then return s else
              uncurry step =<< instruction (cursor prog) prog s

instruction :: Char -> Program -> DataArea -> IO (Program, DataArea)
instruction '<' prog s = return (right prog, left s)
instruction '>' prog s = return (right prog, right s)
instruction '+' prog s = return (right prog, replace (succ $ cursor s) s)
instruction '-' prog s = return (right prog, replace (pred $ cursor s) s)
instruction '.' prog s = putStr [cursor s] >> return (right prog, s)
instruction ',' prog s = fmap ((,) (right prog) . flip replace s) getChar
instruction '[' prog s = return $ (if cursor s == '\NUL' then
                         right $ move right '[' ']' prog else right prog, s)
instruction ']' prog s = return (move left ']' '[' prog, s)
instruction _   prog s = return (right prog, s)

move :: (Program -> Program) -> Char -> Char -> Program -> Program
move dir open close = f 0 . dir where
    f 0 z | cursor z == close = z
    f n z = f (if cursor z == open  then n + 1 else
               if cursor z == close then n - 1 else n) $ dir z

main :: IO ()
main = run "++++++++++[>+++++++>++++++++++>+++>+<\
 \<<<-]>++.>+.+++++++..+++.>++.<<++++++\
 \+++++++++.>.+++.------.--------.>+.>." >> return ()


