; Bubble sort
        ; Initialize a list
        SET [0x1000], 82
        SET [0x1001], 10
        SET [0x1002], 62
        SET [0x1003], 50
        SET [0x1004], 4
        SET [0x1005], 20
        SET [0x1006], 77
        SET [0x1007], 6
        SET [0x1008], 28
        SET [0x1009], 17

        ; Call sort
        SET PUSH, 0x1000
        SET PUSH, 10
        JSR sort

        ; Stop
        SET PC, crash


; Sort a list
:sort   SET Z, POP  ; Return address
        SET B, POP  ; List size
        SET A, POP  ; List address
        SET J, 1    ; Flag indicating if we did any swaps

:sort1  ; Check J and start inner loop
        IFE J, 0
        SET PC, Z
        SET I, B
        SET J, 0

:sort2  ; Iterate (inner loop)
        SUB I, 1
        IFE I, 0
        SET PC, sort1

        ; Compare
        SET X, A
        ADD X, I
        SET Y, X
        SUB X, 1
        IFG [Y], [X]
        SET PC, sort2

        ; Swap
        SET C, [Y]
        SET [Y], [X]
        SET [X], C
        SET J, 1
        SET PC, sort2


:crash  SET I, I
