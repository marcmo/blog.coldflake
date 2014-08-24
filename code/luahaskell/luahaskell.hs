module Main where

import qualified Scripting.Lua as Lua
import Text.Printf
import Data.Word
-- import Data.Char
import Control.Concurrent(threadDelay)
import Numeric
-- import Foreign.C
-- import Foreign.Ptr
import Control.Monad

lua_noerrors = 0
lua_yield = 1
lua_errrun = 2
lua_errsyntax = 3
lua_errmem = 4

main = executeLuaScript "script.lua"

reportError :: Lua.LuaState -> String -> IO()
reportError s desc = do
      nr <- Lua.gettop s
      printf "%d elements on the stack\n" nr
      err <- Lua.tostring s (-1)
      Lua.pop s 1 -- remove error message
      error $ desc ++ " - " ++ err

dofile :: Lua.LuaState -> String -> IO ()
dofile s name = do
    res <- Lua.loadfile s name
    -- check res
    let handlePcall x
          | x == lua_noerrors = printf "executed file correctly: %s\n" name
          | x == lua_errrun = reportError s "run-error"
          | x == lua_errsyntax = reportError s "syntax-error"
          | x == lua_errmem = reportError s "memory-allocation-error"
          | otherwise = reportError s (printf "unknown error(code %d) when executing file: %s\n" x name)
    Lua.pcall s 0 0 0 >>= handlePcall
      where check res = unless (res == lua_noerrors) (error $ "could not load file:" ++ name)

dostring :: Lua.LuaState -> (Int,Int) -> String -> IO Int
dostring s (params,returns) str = do
    res <- Lua.loadstring s str ""
    Lua.pcall s 0 params returns
    return res

executeLuaScript script = do
    s <- Lua.newstate
    Lua.openlibs s

    Lua.registerhsfunction s "send" hsSend
    Lua.registerhsfunction s "wait" hsSleep

    dofile s script

    dostring s (1,1) "return ip_address"
    ipPresent <- Lua.isnil s (-1)
    if ipPresent
      then print "no ip_address defined"
      else do
          ip <- Lua.tostring s (-1)
          print ip
    Lua.pop s 1
    d <- Lua.gettop s
    print $ "top: " ++ show d
    Lua.close s

string2hex ::  String -> Word8
string2hex = fst . head . readHex

hsSend :: String -> IO String
hsSend ip2 = do
  printf "sending %s\n" ip2
  return "hi from send"

hsSleep :: Int -> IO ()
hsSleep n = do
  printf "waiting %d ms\n" n
  threadDelay(1*1000*n)


