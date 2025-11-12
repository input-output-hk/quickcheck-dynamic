# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to  [Semantic Versioning](https://semver.org/).

As a minor extension, we also keep a semantic version for the `UNRELEASED`
changes.

## UNRELEASED

* **BREAKING**: Removed `m` parameter from `PostConditionM` as this is not generally safe.
* **BREAKING**: Additional `Show` constraint on the result of actions
* A new module `Test.QuickCheck.ParallelActions` that implements testing linearizability of a model
  when running in parallel.
* Additional `Property` combinators `sometimes` and `always`

## 4.0.0 - 2025-03-12

* **BREAKING**: Removed `Realized`
  - To migrate uses of `Realized` with `IOSim`, index the state type on the choice of `RunModel` monad
    and index the relevant types:
    ```
    -- Turn:
    data ModelState = State { threadId :: Var ThreadId }
    -- Into:
    data ModelState m = State { threadId :: Var (ThreadId m) }
    ```
* **BREAKING**: Moved `Error state` from `StateModel` to `RunModel` and indexed it on both the `state` and the monad `m`
* **BREAKING**: Changed `PerformResult` from `PerformResult (Error state) a` to `PerformResult state m a`
* Added a `moreActions` property modifier to allow controlling the length of action sequences.

## 3.4.1 - 2024-03-22

* [#70](https://github.com/input-output-hk/quickcheck-dynamic/pull/70) Expose `IsPerformResult` typeclass

## 3.4.0 - 2024-03-01

* Added some lightweight negative-shrinking based on a simple dependency analysis.
* Added the option to return errors from actions by defining `type Error state`.
  When this is defined `perform` has return type `m (Either (Error state) (Realized m a))`,
  when it is left as the default the type remains `m (Realized m a)`.
* Changed `withGenQ` to _require_ a predicate when defining a `Quantification`. **Note**: This is technically a breaking change as the interface changed

## 3.3.1

* Adapt code to _not_ constrain [mtl](https://hackage.haskell.org/package/mtl) version too much

## 3.3.0

* Added suppport for GHC 9.6.2 compiler

## 3.2.0

* Added support for negative testing via `validFailingAction` and `postconditionOnFailure`
  callbacks in `StateModel` and `RunModel`.

## 3.1.1 - 2023-06-26

* Added instances for `HasVariables` with custom error messages to avoid the issue of
  missing `Generic` instances causing difficult to understand type errors.

## 3.1.0 - 2023-04-10

* **BREAKING**: Change the type of `postcondition` to allow you to
  express property monitoring (e.g. stats or counterexamples) in the
  postcondition itself - rather than duplicating code for counterexamples
  in the `monitoring` function.

## 3.0.3 - 2023-04-18

* Added `hasNoVariablesQ` and `forAllNonVariableDL` functions to help make
  quantification require less boilerplate in `DL` properties.

## 3.0.2 - 2023-02-17

* Added instances of `HasVariables` for Word types
* Exported definition of `HasNoVariables` to make it useable
  with deriving via in downstream packages (whoops!)
* Fixed impossible to use `nextVar` arguments to `forAllUniqueDL`

## 3.0.1 - 2023-02-15

* Remove template haskell dependency

## 3.0.0 - 2023-02-14

* **BREAKING**: Add `HasVariables` class to keep track of symbolic variables and automatically insert precondition
  checks for well-scopedness of variables.
* **BREAKING**: Remove some unnecessary and unusead features in dynamic logic, including re-running tests from a
  counterexample directly.
* Improved printing of counterexamples in DL - they are now printed as code that can be copied more-or-less verbatim to
  create a runnable counterexample in code.
* Made the variable context explicit to avoid having to keep track of symbolic variables in the model
  * This introduces the `ctxAtType` and `arbitraryVar` functions to use in action generators (c.f. the
  `RegistryModel.hs` example).

## 2.0.0 - 2022-10-11

* **BREAKING**: Add `Realized` type family to distinguish between the model- and real type of an action
* **BREAKING**: Introduce `RunModel` type class to interpret Model-generated sequence of actions against real-world implementation
  * Move `perform` method from `StateModel` to this new type-class
  * Also split `postcondition` and `monitoring` out from the `StateModel` to the `RunModel` type class
* Added Thread registry example based on io-sim concurrency simulation library

## 1.1.0 - 2022-08-27

* Fix broken links in Hackage-generated documentation and link to other Quviq papers
* Add `Show a` constraint on `monitoring`

## 1.0.0

* Initial publication of quickcheck-dynamic library on Hackage
* Provide base `StateModel` and `DynamicLogic` tools to write quickcheck-based models, express properties, and test them
