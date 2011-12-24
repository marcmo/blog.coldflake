import qualified Scripting.Lua as Lua

main = executeLuaScript "script02.lua"

executeLuaScript ::  String -> IO ()
executeLuaScript script = do
    s <- Lua.newstate
    Lua.openlibs s
    ip <- getIp s script
    maybe (print "no ip found") print ip
    Lua.close s

getIp :: Lua.LuaState -> String -> IO (Maybe String)
getIp s script = do
    Lua.loadfile s script >> Lua.pcall s 0 0 0
    Lua.getglobal s "ip_address"
    ipPresent <- Lua.isstring s (-1)
    if ipPresent
      then Lua.tostring s (-1) >>= return . Just
      else return Nothing

