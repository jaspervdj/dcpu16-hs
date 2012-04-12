; A 32 bit add of 0x12345678 and 0xaabbccdd
SET [0x1000], 0x5678    ; low word
SET [0x1001], 0x1234    ; high word
ADD [0x1000], 0xccdd    ; add low words, sets O to either 0 or 1 (in this case 1)
ADD [0x1001], O         ; add O to the high word
ADD [0x1001], 0xaabb    ; add high words, sets O again (to 0, as 0xaabb+0x1235 is lower than 0x10000)
