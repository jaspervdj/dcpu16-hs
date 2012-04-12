; X <- sum [n * n | n <- [0 .. 50]]
; When finished, X should be 42925 (0xa7ad)
        SET X, 0   ; Sum accumulator
        SET I, 0   ; Iterator value
        SET Y, 50  ; Max value

:loop   SET A, I
        MUL A, A
        ADD X, A
        ADD I, 1
        IFG I, Y
        SET PC, done
        SET PC, loop

:done   SET I, I
