# Floorplanning

Implementation of [_"Floorplan Design of VLSI Circuits"_](https://eecs.wsu.edu/~daehyun/teaching/2014_EE582/papers/fl-polish.pdf).

This code is part of the final project from [Algorithms for AVLSI](https://www.fib.upc.edu/en/studies/masters/master-innovation-and-research-informatics/curriculum/syllabus/AVLSI-MIRI) subject (master's degree in computer science from UPC).

![Floorplan gif](./demo.gif)

## Installation

This project is build using [GHC](https://www.haskell.org/ghc/)(compiler) and [cabal](https://cabal.readthedocs.io/en/latest/index.html)(build tool).

The easiest way to install both is using [ghcup](https://gitlab.haskell.org/haskell/ghcup-hs)

``` sh
# Install ghcup
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

# Install GHC using ghcup
ghcup install ghc 8.8.4

# Install cabal using ghcup
ghcup install cabal
```

Finally, we need to compile the project. This may takes some minutes and requires internet connection. This project does not depend on any `.so` so it should be possible to compile it in any architecture that supports `ghc`.

```sh
# It may takes some minutes
$ cabal build
...
Preprocessing executable 'floorplanning' for floorplanning-0.1.0.0..
Building executable 'floorplanning' for floorplanning-0.1.0.0..
```

## Usage

Let's start by showing the available options:

```sh
# Use the --help flag to display the options
$ cabal run floorplanning -- --help

Usage: floorplanning ((-f|--file FILE) | --demo INTEGER)
                     [--aspect-ratio (DOUBLE, DOUBLE)] [--lambda [0.0,1.0]]
                     [--cooling-rate (0.0,1.0)] [--gamma (1,100)]

Available options:
  -f,--file FILE           Input file
  --demo INTEGER           Run demo with the given number of modules
  --aspect-ratio (DOUBLE, DOUBLE)
                           Aspect Ratio Constraint (default: (0.5,2.0))
  --lambda [0.0,1.0]       Lambda Parameter: area + lambda*wirelength
                           (default: 0.5)
  --cooling-rate (0.0,1.0) Cooling Rate Parameter: simulated annealing
                           (default: 0.85)
  --gamma (1,100)          Gamma Parameter: simulated annealing (default: 5)
  -h,--help                Show this help text
```

You can either try the demo:

```bash
# Replace `n` by the number of modules used for the demo.
# n ~ 50 is recommended.
cabal run floorplanning -- --demo n
```

or one of the examples files from `examples/`:

```bash
$ cabal run floorplanning -- -f "examples/example_20_modules"
============ Report ============
┌                               ┐
│  2  0  0 20 20 20 20 17 17  0 │
│  9  9  9 20 20 20 20 17 17  0 │
│  4  4  0  1  1  1  1 18 18 18 │
│  4  4  0  1  1  1  1 18 18 18 │
│  3  3  6 19 19 19 19 18 18 18 │
│ 11 11  6 12 12 15  0 10 16 16 │
│ 11 11  6 12 12 15  0 14 16 16 │
│ 11 11  6 12 12 15 13 14 16 16 │
│  5  5  5  7  7 15 13 14 16 16 │
│  8  8  8  7  7 15 13 14 16 16 │
└                               ┘

+----++----+-----+------------+-------+-------+
|    ||  n |   λ | Total Area |     A |     W |
+====++====+=====+============+=======+=======+
| P1 || 20 | 0.0 |       92.0 | 100.0 | 157.5 |
+----++----+-----+------------+-------+-------+
```

## Structure

- `app` contains the sources of the executable.
- `src` contains the sources of the library that the executable depends on.
- `test` contains the sources of the library's test.
- `slides` contains the sources for the presentation.
- `examples` contains some examples to test the application.
- `nix` contains the sources for the nix building tool.

## Future Work

The second part, L-Shapes modules, is not implemented.
