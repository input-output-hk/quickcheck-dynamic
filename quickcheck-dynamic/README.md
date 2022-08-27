# quickcheck-dynamic

A library for testing stateful programs using [QuickCheck](https://hackage.haskell.org/package/QuickCheck) and [dynamic logic](https://en.wikipedia.org/wiki/Dynamic_logic_(modal_logic)).

## Background

This library was initially designed by [QuviQ](http://www.quviq.com/) in collaboration with
[IOG](https://iohk.io/) to provide a dedicated test framework for [Plutus](https://docs.cardano.org/plutus/learn-about-plutus) "Smart
contracts". As the need of a _Model-Based Testing_ framework arises in
quite a lot of contexts, it was deemed useful to extract the most
generic part as a standalone package with no strings attached to
Plutus or Cardano.

## Usage

* Documentation is currenly mostly provided inline as Haddock
  comments. Checkout [StateModel](https://hackage.haskell.org/package/quickcheck-dynamic/docs/src/Test.QuickCheck.StateModel.html)
  and [DynamicLogic](https://hackage.haskell.org/package/quickcheck-dynamic/docs/Test-QuickCheck-DynamicLogic.html) modules for
  some usage instructions.
* For a concrete standalone example, have a look at `Registry` and `RegistryModel` modules from the companion [quickcheck-io-sim-compat](https://github.com/input-output-hk/quickcheck-dynamic/tree/main/quickcheck-io-sim-compat) package (not currently available on hackage), a multithreaded Thread registry inspired by the Erlang version of QuickCheck described in [this article](https://mengwangoxf.github.io/Papers/Erlang18.pdf)
* For more documentation on how to quickcheck-dynamic is used to test
  Plutus DApps, check this
  [tutorial](https://plutus-apps.readthedocs.io/en/latest/plutus/tutorials/contract-models.html).
* Apart from Plutus, this library is now in use in the
  [Hydra](https://github.com/input-output-hk/hydra-poc) project to
  verify the _Head Protocol_ implementation with respect to the
  original research paper.
