(appendFile ".gitignore") .  -- 4
unlines .                    -- 3
filter((flip(=~)) ".*txt") . -- 2
lines                        -- 1
