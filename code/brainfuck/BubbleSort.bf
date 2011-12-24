>>>                         create a buffer

,+[->,+]                    read input (eof is minus 1)
<[<]>                       rewind to first value (c)
>                           cursor to d

[                           until we're done:

    [<                      cursor to c (base cursor pos)
        [                   if c is not 0:

            <<+>> - >-<     inc a; dec c and d
            [               if c is still not 0 swap:

                >           cursor to d
                [-<<+>>]    moves d to b
                <           cursor to c
                [->+<]      move c to d
            ]               done or c was 0
            <[->+<]>        move b to c

        ]                   when c is 0:
        <<                  cursor to a
        [->+>>+<<<]>[-<+>]  move a to b and add a to d
                            and move b back to a
        >>                  cursor to c

        >                   cursor to d (checking if done)
    ]                       pass is done
    
    <[->+<]<<<              cursor to c
                            move c to d
                            cursor to a
                        
    [[->>+<<]<]>>>          until a is 0:
                            move a to c
                            cursor to previous a

                            cursor to c                     
]                           if c is 0 we're done
>[.>]                       cursor to d and output values

