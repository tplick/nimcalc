**nimcalc**

Tom Plick (tomplick@gmail.com)

nimcalc, the "nimber calculator," is a software package designed to compute the nim-values of impartial games.  So far, three games are implemented:

- Cram, a game of dominoes played on a square grid.  See https://en.wikipedia.org/wiki/Cram_(game) for information on this game.  This game is implemented in cram.ml .  For added speed, the nim-values of all boards up to 4 x 6 have been precomputed and stored in the file cram4by6.db .

- A two-player version of the n-queens problem.  In this game, players alternate placing queens on an n x n chessboard, such that no queen attacks any other.  The player left with no move is the loser.  This game is implemented in queens.ml .

- Wythoff's game, a variant of Nim.  There are two heaps of beans, and a valid move consists of removing beans from one heap or the same number of beans from both heaps.  This game is implemented in beans.ml .  If you want to implement a new game in nimcalc, looking at the implementation of this game is a good place to start.

nimcalc is implemented in OCaml.  To run the code, follow these steps:

- Install opam from https://ocaml.org/install .  Then, install OCaml 5.2.1 by executing:

`opam switch create 5.2.1`

- Activate this version of the compiler by executing

```bash
opam switch 5.2.1
eval $(opam env)
```

- Compile the code by executing `make`.

- As a test, run

`./cram test`

You will see several rows like this:

`3 x 6: 4`

This means that the nim-value of the 3 x 6 Cram board is 4.


**Runtime options**

You can modify the behavior of the program by setting environment variables.

Setting NIMCALC_VERBOSE=1 will cause the program to print out information as it is running.

Setting NIMCALC_PROCS=[n] will make the program use n subprocesses.  Ideally this will reduce the time taken by a factor of n, but some circumstances can cause the speedup to be less.

Setting NIMCALC_TABLE_SIZE=[n] sets the size of each transposition table to n entries.  The value of n is rounded up to the nearest power of two.  Each level of evaluation in the tree is assigned its own transposition table.  The default size is 8192 and is suitable for running experiments on a typical laptop or desktop.  If you are performing a long-running computation on a powerful server with a large amount of free RAM, it is suggested to set NIMCALC_TABLE_SIZE to 64000.  This often reduces running time by a large amount, close to 50% in some cases.

```bash
tom@Toms-MacBook-Air nimcalc % time NIMCALC_VERBOSE=1 ./cram 4 7
Using default table size of 8192.
Trying nimber 1...
  There are 17 options to try.
  Trying option #3...
Trying nimber 2...
  There are 18 options to try.
  Trying option #4...
Trying nimber 3...
  There are 19 options to try.
  Trying option #19...
4 x 7: 3  (6.77 sec, 9972015 positions, 1115744 HT hits, 818074 splits)
NIMCALC_VERBOSE=1 ./cram 4 7  6.76s user 0.02s system 99% cpu 6.779 total
tom@Toms-MacBook-Air nimcalc % time NIMCALC_VERBOSE=1 NIMCALC_PROCS=4 ./cram 4 7
Using default table size of 8192.
Trying nimber 1...
  There are 17 options to try.
  15 left...   
Trying nimber 2...
  There are 18 options to try.
  13 left...   
Trying nimber 3...
  There are 19 options to try.
  0 left...    
4 x 7: 3  (0.02 sec, 0 positions, 0 HT hits, 0 splits)
NIMCALC_VERBOSE=1 NIMCALC_PROCS=4 ./cram 4 7  10.11s user 0.11s system 383% cpu 2.667 total
```

In multiprocess mode, the time and the counts (positions, HT hits, splits) shown at the end are not accurate.  Use your system's `time` command to get an accurate timing.  In the example above, using 4 processes reduced the time taken (in terms of the wall clock) from 6.8 seconds to 2.7 seconds.

This is the first version of the Readme, and I still have much more information to add.  For now, if you have questions, feel free to email me at the address listed above.
