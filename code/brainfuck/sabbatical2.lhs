

> module Main where
>
> import qualified Control.Exception as CE
> import Control.Monad
> import Data.Char
> import Data.Word
> import System.Environment
> import System.IO
> import Text.ParserCombinators.Parsec

> import Debug.Trace


This version of interpreter makes use of "precompiled" and optimized Brainfuck code. Source file gets compiled to following set of operations:


> data Op a =
>     Add a         -- add a to current memory cell (a can be negative)
>   | Move a        -- move pointer a positions to the right (to the left if a negative)
>   | Input         -- input one character and store it in memory
>   | Output        -- output current memory cell to stdout
>   | Loop [Op a]   -- loop with body consisting of given operations list
>   | Set a         -- OPTIMIZATION: set cell to value
>   | FarAdd a a    -- OPTIMIZATION: add k*value of current cell to cell 'a' away
>   | Scan a        -- OPTIMIZATION: search for 0 jumping x cells at a time
>       deriving (Show, Eq)


Source code parsing is done using simple grammar written with Parsec.

The BNF form of grammar looks something like this:

program     ::= instruction*
instruction ::= loop | simple
loop        ::= '[' instruction* ']'
simple      ::= '+' | '-' | '<' | '>' | '.' | ','

which can be written in Parsec:


> program :: Parser [Op Int]
> program = many instruction
>
> instruction :: Parser (Op Int)
> instruction = simple <|> loop
>
> loop :: Parser (Op Int)
> loop = between (char '[') (char ']') program >>= \p -> return $ Loop p
>
> simple :: Parser (Op Int)
> simple = (many1 (char '+') >>= \p -> return $ Add (length p))
>      <|> (many1 (char '-') >>= \p -> return $ Add (negate $ length p))
>      <|> (many1 (char '>') >>= \p -> return $ Move (length p))
>      <|> (many1 (char '<') >>= \p -> return $ Move (negate $ length p))
>      <|> (      (char '.') >>= \_ -> return $ Output)
>      <|> (      (char ',') >>= \_ -> return $ Input)


Data memory is handled using a zipper:


> data ListZipper a = ListZipper {
>   left    :: ![a], -- elements left from focus
>   focus   :: ! a , -- current element
>   right   :: ![a]  -- elements right from focus
> } deriving Show
>
> move :: Int -> ListZipper a -> ListZipper a
> move (-1) (ListZipper (x:xs) y zz)  = ListZipper xs x (y:zz)
> move   1  (ListZipper xx y (z:zs))  = ListZipper (y:xx) z zs
> move   0  lz                        = lz
> move   n  lz = if n > 0 then move (n-1) (move 1 lz)
>                         else move (n+1) (move (-1) lz)
>
> mkZipper :: [a] -> ListZipper a
> mkZipper x = ListZipper [] (head x) (tail x)
>
> getValue :: ListZipper a -> a
> getValue (ListZipper _ y _) = y
>
> setValue :: ListZipper a -> a -> ListZipper a
> setValue (ListZipper xx _ yy) v = ListZipper xx v yy
>
> {-# INLINE move     #-}
> {-# INLINE getValue #-}
> {-# INLINE setValue #-}

> scan :: Int -> ListZipper Word16 -> ListZipper Word16
> scan n lz@(ListZipper _ 0 _) = lz
> scan n lz = scan n (move n lz)
> 
> addAt :: Int -> Int -> ListZipper Word16 -> ListZipper Word16
> addAt n k lz@(ListZipper l v r) =
>       if v == 0 then lz
>       else let doAddAt 0 _ _ = error "Invalid use of doAddAt - 0 offset"
>                doAddAt 1 (x:xs) v = (v+x):xs
>                doAddAt n (x:xs) v = x:(doAddAt (n-1) xs v)
>                value  = fromIntegral k*v in
>            if n > 0 then ListZipper l v (doAddAt n r value)
>            else ListZipper (doAddAt (-n) l value) v r
>
> {-# INLINE addAt #-}
> {-# INLINE scan  #-}



Time for main interpreter.

Executing program transforms memory state (ListZipper Word16) into a new state with possible side-effects in I/O.


> runSequence :: ListZipper Word16 -> [Op Int] -> IO (ListZipper Word16)
> runSequence memory = foldM step memory
>
> step :: ListZipper Word16 -> Op Int -> IO (ListZipper Word16)
> step mem op = --trace (show op) $!
>   case op of
>       Move n      -> return $! move n mem
>       Add n       -> return $! setValue mem ((getValue mem) + (fromIntegral n))
>       Loop p      -> doLoop p mem
>       Set n       -> return $! setValue mem (fromIntegral n)
>       FarAdd n k  -> return $! addAt n k mem
>       Scan n      -> return $! scan n mem
>       Input       -> getChar >>= \x -> return $! setValue mem (fromIntegral $ ord x)
>       Output      -> hPutChar stdout (chr . fromEnum $ getValue mem) >> return mem


I have enclosed loop body execution into a separate function to make it look more clear (for me at least). Algorithm is simple - if on entry to the loop current memory cell has non-zero value, the loop body is executed as if it was a separate program, and the resulting memory state is passed again to doLoop to check if another iteration should be performed.
If value of current cell is zero program is skipped and function simply returns the same memory state that it got as parameter.


> doLoop :: [Op Int] -> ListZipper Word16 -> IO (ListZipper Word16)
> doLoop block memory = if (getValue memory) == 0 then return memory
>                                                 else runSequence memory block >>= doLoop block


Optimization step - precompiled program is transformed according to following rules:

sequence of Add operations is replaced with single Add with argument being sum of arguments of input, for example [Add 1, Add1] will be replaced with [Add 2], [Add 1, Add 1, Add (-1)] with [Add 1] etc. Technically sequences of '+' or '-' are handled by Brainfuck parser, but it does not optimize intermixed '+' and '-' correctly, for example in "+-+-+-". This is actually programer's mistake, but can happen anyway.

sequence of Move operations is replaced with single Move in similar fashion - [Move (-1), Move (-1)] is replaced with [Move (-2)] etc. Situation here is similar to described above - parser does not optimize "<><><><>" properly.

empty operations are removed, for example Add 0, Move 0 or empty loop.


Optimizer also replaces common loop patterns with single opcodes:

Loop [Add (-1)] (filling current cell with 0) is replaced with Set 0.

Loop [Add (-1), Move n, Add k, Move (-n)] (addition of k*current to cell n away) replaced with [FarAdd n k, Set 0] (Set 0 is necessary, since side-effect of the original loop is setting current cell to 0). Also variant with Add (-1) at the end of the loop is recognized.

Loop [Add (-1), Move n1, Add k1, Move n2, Add k2, Move n3] if n1+n2+n3 == 0 (two additions - k1*current to cell n1 away, k2*current to cell n1+n2) replaced with sequence of [FarAdd n1 k1, FarAdd (n1+n2) k2, Set 0]. Again, also variant with Add (-1) at the end is recognized.




> optimize :: [Op Int] -> [Op Int]
> optimize []                       = []
> optimize ((Add 0)          :xs)   = optimize xs
> optimize ((Move 0)         :xs)   = optimize xs
> optimize ((Loop [])        :xs)   = optimize xs
> optimize ((Add x) :(Add y) :xs)   = optimize (Add (x+y) :xs)
> optimize ((Move x):(Move y):xs)   = optimize (Move (x+y):xs)
> optimize ((Set x) :(Add y) :xs)   = optimize (Set (x+y) :xs)
> optimize ((Set x) :(Set y) :xs)   = optimize (Set y     :xs)
> optimize ((Loop [Add (-1)]):xs)   = optimize (Set 0     :xs)
> optimize ((Loop [Move x])  :xs)   = optimize (Scan x    :xs)
> optimize ((Loop p)         :xs)   = let p' = optimize p in loopOptimize (Loop p') ++ (optimize xs)
> optimize (x                :xs)   = x:(optimize xs)
> 
> loopOptimize x@(Loop [Add (-1), Move n1, Add k, Move n2]) =
>       if n1 == -n2 then [FarAdd n1 k, Set 0]
>                    else [x]
> loopOptimize x@(Loop [Move n1, Add k, Move n2, Add (-1)]) =
>       if n1 == -n2 then [FarAdd n1 k, Set 0]
>                    else [x]
> loopOptimize x@(Loop [Add (-1), Move n1, Add k1, Move n2, Add k2, Move n3]) =
>       if (n1+n2) == -n3 then [FarAdd n1 k1, FarAdd (n1+n2) k2, Set 0]
>                         else [x]
> loopOptimize x@(Loop [Move n1, Add k1, Move n2, Add k2, Move n3, Add (-1)]) =
>       if (n1+n2) == -n3 then [FarAdd n1 k1, FarAdd (n1+n2) k2, Set 0]
>                         else [x]
> loopOptimize x = [x]


doOptimize does optimization on its input as long as subsequent optimize calls result in any changes to the program.


> doOptimize p = let p' = optimize p in
>                   if p == p' then p else doOptimize p'


Program "normalization", i.e. removal of all characters other than valid Brainfuck instructions.


> normalize :: String -> String
> normalize program = filter (`elem` "+-<>[].,") program


And finally main function.


> main :: IO ()
> main = do
>       args <- getArgs
>       if length args < 1
>           then fail "Please provide name of the program to run"
>           else do
>               prog <- readFile (head args)
>               case (parse program "" . normalize) prog of
>                   Left err    -> do putStr "Parse error at "
>                                     print err
>                   Right res   -> do
>                                   runSequence (mkZipper (replicate 30000 0)) (doOptimize res)
>                                   return ()


