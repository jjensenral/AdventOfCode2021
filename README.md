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

To avoid creating lots of subdirectories, and loads of unnecessary files, generally everything is kept in a single file, except that

- some shared code is factored out into `util.lisp`
  - unlike last year, we try harder to keep it backwards compatible as it gets updated
- the occasional external dependency
  - widely available code (i.e. already in your distro) is strongly preferred

### Performance

The code is written to solve a single problem and to some extent the whole premise rewards speed over, say, reusability or elegance.  Performance issues - like avoiding consing or optimising tail recursion - is usually not an issue, unless indicated in comments (cf last year's "go faster stripes").

## References

1. Guy L. Steele's Common Lisp the Language, 2nd Edition, predates the Standard but is great fun and a useful reference.
2. Lispworks' HyperSpec, absolutely essential reference
