# Advent of Code 2022

Solutions to https://adventofcode.com/2022 in two languages (so far):

- [Clojure](https://clojure.org/) in `src/gmorpheme/aoc22`
- [Eucalypt](https://curvelogic.github.io/eucalypt/) in `eu/`

Clojure for speed and brevity, Eucalypt, following along afterwards
because I created it and this is a great means to iron out bugs,
kinks, and prioritise features.

On the clojure side, these are just for running in the REPL, there's
no kind of main or harness.

On the eucalypt side... thing is, right now, eucalypt:
- has no data structures apart from lists ("blocks" are just alists
  restricted to symbol keys); in particular no sets or hash tables
- has no real type system to speak of
- has broken error messages
- has no GC yet (though it's in progress)
- has quite a messy "prelude" (i.e. standard library)
- has no escape hatch to a host language
- was never actually intended to be a general purpose programming language.

...all of which means way more ingenuity is required to solve the
problems, and sometimes means the `eu` solution is very different from
the `clj` solution.

They can all be run with e.g. `eu day1.eu` in the `eu` directory,
though some might depend on pre-release versions if I don't keep up
with releases.

For anyone interested,
[eucalypt](https://github.com/curvelogic/eucalypt) is a rather
eccentric pure functional language, written in Rust. Install `eu` on
MacOS with `brew install curvelogic/homebrew-tap/eucalypt`.
