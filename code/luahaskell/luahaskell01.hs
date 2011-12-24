import qualified Scripting.Lua as Lua

main ::  IO ()
main = do
    s <- Lua.newstate
    Lua.openlibs s
    Lua.loadfile s "script.lua"
    Lua.pcall s 0 0 0
    Lua.close s

