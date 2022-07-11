# quickcheck-dynamic

A library for testing stateful programs using [QuickCheck](https://hackage.haskell.org/package/QuickCheck) and [dynamic logic](https://en.wikipedia.org/wiki/Dynamic_logic_(modal_logic)).

<div align="center">
  <a href='https://github.com/input-output-hk/quickcheck-dynamic/actions'><img src="https://img.shields.io/github/workflow/status/input-output-hk/hydra-poc/CI?label=Tests&style=for-the-badge" /></a>
</div>

> :warning: :warning: :warning:
>
> This is still work-in-progress.

## Background

This library was initially designed by [QuviQ](http://www.quviq.com/) in collaboration with
[IOG](https://iohk.io/) to provide a dedicated test framework for [Plutus](https://docs.cardano.org/plutus/learn-about-plutus) "Smart
contracts". As the need of a _Model-Based Testing_ framework arises in
quite a lot of contexts, it was deemed useful to extract the most
generic part as a standalone package with no strings attached to
Plutus or Cardano. Apart from Plutus, this library is now in use in
the [Hydra](https://github.com/input-output-hk/hydra-poc) project to
verify the _Head Protocol_ implementation with respect to the original
research paper.

## Usage

* Documentation is currenly mostly provided inline as Haddock
  comments. Checkout [StateModel](src/Test/QuickCheck/StateModel.hs)
  and [DynamicLogic](src/Test/QuickCheck/DynamicLogic.hs) modules for
  some usage instructions.
* For a concrete standalone example, testing a registry for running
  threads, see [Registry](test/Spec/DynamicLogic/Registry.hs) and
  [RegistryModel](test/Spec/DynamicLogic/RegistryModel.hs).
* For more documentation on how to quickcheck-dynamic is used to test
  Plutus DApps, check this
  [tutorial](https://plutus-apps.readthedocs.io/en/latest/plutus/tutorials/contract-models.html)

## Building

### Without nix

This package uses a simple [Cabal](https://www.haskell.org/cabal/)-based build. To build from source:

* Ensure both `ghc` and `cabal` executables are in your `PATH`.
  * [ghcup](https://www.haskell.org/ghcup/) is a great way to manage Haskell toolchain.
  * quickcheck-dynamic currently requires a GHC version > 8.10
* Run
  ```
  cabal update && cabal build
  ```
* To run tests:
  ```
  cabal test
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
