# AdventOfCode2021
Advent of Code done in functional style (mostly)

## Goals

This is an attempt at solving [Advent of Code 2021](https://adventofcode.com/2021)
until I run out of time (last year got a bit too challenging on top of the day job).

Since the aim is to solve the problems, I will do the challenge in Common
Lisp since it is easy to rapidly prototype and usually results in fairly elegant
code (see Day 09 as an example).  It is tested with SBCL on my very slow laptop.  Developed with Emacs and SLIME.

The aim is to use a functional style whenever it makes sense, choosing procedural
only when it is shorter or necessary for some other reason.  A good example of the latter is array code where the lispiest approach is to displace a vector to the array and use sequence functions.

### Structuring code

To avoid creating lots of subdirectories, and loads of unnecessary files, generally every single day is kept in a single file, except that

- some shared code is factored out into `util.lisp`
  - unlike last year, we try harder to keep it backwards compatible as it gets updated
- the occasional external dependency
  - widely available code (i.e. already in your distro) is strongly preferred

When it makes sense, a code file is structured into pages (^L) in order to separate things which a more timid (or tidier) programmer might have put in separate files.

### Performance

The code is written to solve a single problem and to some extent the whole premise of Advent of Code rewards speed (of coding) over, say, reusability or elegance.  Performance - like avoiding consing or optimising tail recursion - is usually not an issue, unless indicated in comments (cf last year's "go faster stripes").  This code is written for

1. Clarity and correctness
2. Reusability (hmm, optimistically), or perhaps didactic value (hmm)
3. Performance

This does not mean performance is unimportant.  Most problems run in tens of milliseconds on the author's old laptop, which was fairly slow even when it was new.

## How to run these examples (example)

Taking Day 7 as a fairly typical example, here's how to run this code with [SBCL](https://www.cliki.net/SBCL).  It assumes you have installed the few dependencies such as split-sequence into the system.  The first time you load split-sequence (or any other package), it is compiled and cached for future use.  The `*` are the SBCL prompts; the following lines show typical output.

```
* (require 'asdf)
("ASDF" "asdf" "UIOP" "uiop")
* (asdf:oos 'asdf:load-op :split-sequence)
#<ASDF/LISP-ACTION:LOAD-OP >
#<ASDF/PLAN:SEQUENTIAL-PLAN {1004D19B63}>
* (load #P"util")
T
* (load #P"07")
T
* (solve1 +day7-test-input+)
37
* (solve2 +day7-test-input+)
168
* (solve1 #P"07.input")
* (solve2 #P"07.input")
```

The numbers for the last two results are not shown.


## References

1. Guy L. Steele's Common Lisp the Language, 2nd Edition, predates the Standard but is great fun and a useful reference.
2. Lispworks' HyperSpec, absolutely essential reference
