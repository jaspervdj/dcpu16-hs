dcpu16-hs
=========

Implementation of Notch's [dcpu-16] CPU in Haskell. This project includes an
assembler and an emulator.

[dcpu-16]: http://0x10c.com/doc/dcpu-16.txt

Status
------

We can assemble and run at least Notch's example program, this [fibonacci]
example, and the stuff in `examples/`.

[fibonacci]: https://github.com/jazzychad/dcpu-asm/blob/master/fib.d16

Like the assembler Notch uses, short form for labels isn't supported yet.

The emulator just runs until it crashes, there is no stopping strategy for now.

Building
--------

    cabal configure && cabal build

Running
-------

    ./dist/build/dcpu16-assembler/dcpu16-assembler examples/notch.s

This produces `a.out`.

    ./dist/build/dcpu16-emulator/dcpu16-emulator a.out

You should probably redirect the output to a file.
