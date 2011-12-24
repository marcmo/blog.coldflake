
reverse .                          -- 6
L.sortBy(compare `on` snd) .       -- 5
map (last.words.head &&& length) . -- 4
L.group . L.sort .                 -- 3
filter((flip(=~)) "^Author") .     -- 2
lines                              -- 1
