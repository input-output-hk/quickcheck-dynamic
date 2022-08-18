# quickcheck-dynamic

A library for testing stateful programs using [QuickCheck](https://hackage.haskell.org/package/QuickCheck) and [dynamic logic](https://en.wikipedia.org/wiki/Dynamic_logic_(modal_logic)).

The original stateful testing approach is described in John Hughes' research paper: [https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quviq-testing.pdf ](https://publications.lib.chalmers.se/records/fulltext/232550/local_232550.pdf).
The dynamic logic addition allows you to specify that after a generated test sequence, the system is able to reach a specific required state. In other words, you can specify that some "good" state is reachable from any possible state.

<div align="center">
  <a href='https://github.com/input-output-hk/quickcheck-dynamic/actions'><img src="https://img.shields.io/github/workflow/status/input-output-hk/hydra-poc/CI?label=Tests&style=for-the-badge" /></a>
</div>

> :warning: :warning: :warning:
>
> This is still work-in-progress.

## Introduction

This repository hosts:
* The core [quickcheck-dynamic](./quickcheck-dynamic) library providing tools for quickchecking stateful models,
* Example of integrating [io-sim](https://github.com/input-output-hk/io-sim)'s Haskell runtime simulator and _quickcheck-dynamic_ to model and test complex multi-threaded application.

## Building

### Without nix

This package uses [Cabal](https://www.haskell.org/cabal/)-based build. To build from source:

* Ensure both `ghc` and `cabal` executables are in your `PATH`.
  * [ghcup](https://www.haskell.org/ghcup/) is a great way to manage Haskell toolchain.
  * quickcheck-dynamic currently requires a GHC version > 8.10
* Run
  ```
  cabal update && cabal build all
  ```
* To run tests:
  ```
  cabal test all
  ```

### With nix

This repository comes with some [nix](https://nixos.org) files which might or might not help hacking on quickcheck-dynamic simpler.
Before you start using nix, please make sure you've configured haskell.nix caching as per [those instructions](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started.html#setting-up-the-binary-cache).

* Building with nix should be as simple as:
  ```
  nix-build -A quickcheck-dynamic.components.library
  ```
* To enter a shell providing basic development tool:
  ```
  nix-shell
  ```
  This can automated using [direnv](https://direnv.net/):
  ```
  direnv allow
  ```
* Then go back to [Without nix](#without-nix) instructions

### External material 

* John Hughes high level talk on how to test Plutus smart contracts using this library: https://youtu.be/V9_14jjJiuQ
* 55 minutes in to this lecure an example of using the state machine formalism: https://www.youtube.com/watch?v=zW3D2iM5uVg
