dcpu16-hs
=========

Implementation of Notch's [dcpu-16] CPU in Haskell.

[dcpu-16]: http://0x10c.com/doc/dcpu-16.txt

Status
------

The emulator seems to run Notch's test program.

The current strategy is to simulate 10000 instructions (*not* cycles). A better
strategy would be to stop when PC ends up after the code, but this would not
work for some hacks (e.g. programs that modify themselves).

Building
--------

    cabal configure && cabal build

Running
-------

    ./dist/build/dcpu16-hs/dcpu16-hs examples/notch.bin

You should probably redirect the output to a file.
