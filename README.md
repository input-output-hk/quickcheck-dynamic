# quickcheck-dynamic

<div align="center">
  <a href='https://github.com/input-output-hk/quickcheck-dynamic/actions'><img src="https://img.shields.io/github/actions/workflow/status/input-output-hk/quickcheck-dynamic/ci.yaml" /></a>&nbsp;
  <a href='https://hackage.haskell.org/package/quickcheck-dynamic/'><img src="https://img.shields.io/hackage/v/quickcheck-dynamic" /></a>
</div>

A library for testing stateful programs using [QuickCheck](https://hackage.haskell.org/package/QuickCheck) and [dynamic logic](https://en.wikipedia.org/wiki/Dynamic_logic_(modal_logic)).

This repository hosts:
* The core [quickcheck-dynamic](./quickcheck-dynamic) library providing tools for quickchecking stateful models,
* Example of integrating [io-sim](https://github.com/input-output-hk/io-sim)'s Haskell runtime simulator and _quickcheck-dynamic_ to model and test complex multi-threaded application.

## Documentation

* The original stateful testing approach is described in John Hughes' research paper: [https://www.cs.tufts.edu/~nr/cs257/archive/john-hughes/quviq-testing.pdf ](https://publications.lib.chalmers.se/records/fulltext/232550/local_232550.pdf)
* The [Registry example](https://github.com/input-output-hk/quickcheck-dynamic/blob/main/quickcheck-dynamic/test/Spec/DynamicLogic/RegistryModel.hs) is a common case study that's been explored in two papers:
  * [How well are your requirements tested?](https://publications.lib.chalmers.se/records/fulltext/232552/local_232552.pdf)
  * and [Understanding Formal Specifications through Good Examples](https://mengwangoxf.github.io/Papers/Erlang18.pdf)
* The dynamic logic addition allows you to specify that after a generated test sequence, the system is able to reach a specific required state. In other words, you can specify that some "good" state is reachable from any possible state.

The following talks provide concrete examples on how this approach is used to test smart contracts in Plutus:
* John Hughes high level talk on how to test Plutus smart contracts using this library: https://youtu.be/V9_14jjJiuQ
* 55 minutes in to this lecture an example of using the state machine formalism: https://www.youtube.com/watch?v=zW3D2iM5uVg&t=3300

The following blog posts and talks provide some more in-depth educational material on quickcheck-dynamic:
* Edsko de Vries wrote a [nice post](https://well-typed.com/blog/2022/09/lockstep-with-quickcheck-dynamic/) to compare `quickcheck-dynamic` with [quickcheck-state-machine](https://hackage.haskell.org/package/quickcheck-state-machine), another library to write model-based tests on top of QuickCheck. This blog post introduces [quickcheck-lockstep](https://github.com/well-typed/quickcheck-lockstep) which provides _lockstep-style_ testing on top of quickcheck-dynamic,
* IOG published an [introductory post](https://engineering.iog.io/2022-09-28-introduce-q-d) on `quickcheck-dynamic`, detailing some rationale and background for this work, and suggesting a step-by-step approach to use it based on some real world experience.
* A [presentation](https://abailly.github.io/slides/model-based-testing-with-quickcheck.html#/title-slide) from [BOBKonf 2024](https://bobkonf.de/2024/en/program.html) which provides a good overview of why one would want to use such a library, how it's been applied in some concrete projects, and some basic understanding of the mechanics.

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

This repository uses nix to provide a development and build environment.

For instructions on how to install and configure nix (including how to enable access to our binary caches), refer to [this document](https://github.com/input-output-hk/iogx/blob/main/doc/nix-setup-guide.md).

If you already have nix installed and configured, you may enter the development shell by running `nix develop`.

* To enter a shell providing a complete haskell toolchain:
  ```
  nix develop
  ```
  This can automated using [direnv](https://direnv.net/):
  ```
  direnv allow
  ```
* Then go back to [Without nix](#without-nix) instructions
