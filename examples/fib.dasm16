; Fill memory with the fibonacci sequence. Source: reddit.
SET A, 1
SET B, 1
SET PEEK, 1
:loop ADD A, B   ;  A is now 2, B is still 1
SET PUSH, A
SET A, B
SET B, PEEK
IFG SP, 11       ; The original program had 10 here because that's how much
                 ; space this program takes. Modified to be 11, so we run
                 ; into a 0x0000 instruction when the program is finished,
                 ; instead of our fibonacci numbers.
SET PC, loop
