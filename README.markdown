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

The emulator just runs until it crashes (usually when it tries to execute a
0x0000 opcode), there is no stopping strategy for now.

Building
--------

    cabal configure && cabal build

Running
-------

    ./dist/build/dcpu16-assembler/dcpu16-assembler examples/notch.s

This produces `a.out`.

    ./dist/build/dcpu16-emulator/dcpu16-emulator a.out

You should probably redirect the output to a file.

Design
------

We use a homogeneous `Memory` type for all kinds of memory the CPU has access
to:

- Special values such as `SP`, `PC` and `O`
- Custom defined (i.e. not in the spec) values such as `SKIP`, `CYCLES`
- Registers (`A`, `B`...)
- RAM

All these values can be accessed if you know its `Address`, which is statically
known for e.g. `SP`, and can be calculated for e.g. the RAM at `0x1000`.

The core of the design is hence a monadic typeclass:

    class Monad m => MonadEmulator m where
        load  :: Address -> m Word16
        store :: Address -> Word16 -> m ()

Currently, there is only one implementation of this class, `STEmulator`, which
is able to simulate programs in a *pure* way (so a program is guaranteed to
always yield the same result!).

When the spec is better defined, an `IOEmulator` will be added which will not be
pure, in order to be able to grab keyboard events and display video.
