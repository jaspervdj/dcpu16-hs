; This is a self-adapting program. It figures out where it is located in the
; memory, copies itself so that the copy directly follows the original. This
; means that once the original program is finished, the copy will run, and, in
; turn, make a copy of itself...
;
; We use a counter to limit the number of copies.
        ; Number of copies to make
        SET I, 9

:start  SET A, PC
        SUB A, 1  ; Because the previous instruction was 1 word long
        SET B, A
        ADD B, end
        SUB B, start

        ; Bail out when enough copies have been made
        IFE I, 0
        SET PC, B
        SUB I, 1

        ; Calculate relative position of loop label
        SET C, loop
        SUB C, start
        ADD C, A

        SET X, A
        SET Y, B

        ; Jump to the newly written instructions when copy is done
:loop   IFE X, B
        SET PC, B

        SET [Y], [X]
        ADD X, 1
        ADD Y, 1

        ; Remember that C is a modified 'loop'
        SET PC, C

:end
