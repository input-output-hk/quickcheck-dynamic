# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/), and this project adheres to  [Semantic Versioning](https://semver.org/).

As a minor extension, we also keep a semantic version for the `UNRELEASED`
changes.

## UNRELEASED


## 1.1.0 - 2022-08-27

* Fix broken links in Hackage-generated documentation and link to other Quviq papers
* Add `Show a` constraint on `monitoring`

## 1.0.0

* Initial publication of quickcheck-dynamic library on Hackage
* Provide base `StateModel` and `DynamicLogic` tools to write quickcheck-based models, express properties, and test them

## 2.0.0

* Add `Realized` type family to distinguish between the model- and real type of an action
* Introduce `RunModel` type class for `perform`
* Split `postcondition` and `monitoring` out from the `StateModel` to the `RunModel` type class
