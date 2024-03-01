# Contributing to quickcheck-dynamic

Thanks for considering contributing to the development of quickcheck-dynamic.

The best way to contribute right now is to try things out and provide feedback,
but we also accept contributions to the documentation and the obviously to the
code itself.

This document contains guidelines to help you get started and how to make sure
your contribution gets accepted.

## Communication channels

Should you have any questions or need some help in getting set up, you can use Github [Discussions](https://github.com/input-output-hk/quickcheck-dynamic/discussions)
to reach out to the team before submitting issues.

## Your first contribution

Contributing to the documentation, reporting bugs or proposing features are awesome ways to get started.

### Issues

Whether reporting a bug or requesting a new feature, use GitHub to [submit an issue](https://github.com/input-output-hk/quickcheck-dynamic/issues/new/).

For bug reports, it's very important to explain
* what version you used,
* steps to reproduce (or steps you took),
* what behavior you saw (ideally supported by logs), and
* what behavior you expected.

For feature requests or ideas, we expect a description of:
* why you (or the user) need/want something (e.g. problem, challenge, pain, benefit), and
* what this is roughly about.

Note that we do NOT require a detailed technical description, but are much more
interested in *why* a feature is needed. This also helps in understanding the
relevance and ultimately the priority of such an item.

## Making changes

When contributing code, it helps to have discussed the rationale and (ideally)
how something is implemented in a feature idea or bug ticket beforehand.

### Building & Testing

See the [README.md](./README.md#building) file for instructions on how to build and test this package.

### Creating a pull request

Thank you for contributing your changes by opening a pull requests! To get
something merged we usually require:
+ Description of the changes - if your commit messages are great, this is less important
+ Quality of changes is ensured - through new or updated automated tests
+ Change is related to an issue, feature (idea) or bug report - ideally discussed beforehand
+ Well-scoped - we prefer multiple PRs, rather than a big one

### Coding Standards

* Ensure your code is formatted using [fourmolu](https://github.com/fourmolu/fourmolu) with the provided [configuration file](./fourmolu.yaml).

### Versioning & Changelog

During development
+ Make sure `CHANGELOG.md` is kept up-to-date with high-level, technical, but user-focused list of changes according to [keepachangelog](https://keepachangelog.com/en/1.0.0/)
+ Bump `UNRELEASED` version in `CHANGELOG.md` according to [Semantic Versioning](https://semver.org/)

### Releasing

To perform a release (requires maintainers' rights on the repository):
+ Check version to be released is also correct in software components, e.g. `.cabal` files.
+ Replace `UNRELEASED` with a date in [ISO8601](https://en.wikipedia.org/wiki/ISO_8601)
+ Create a signed, annotated git tag of the version: `git tag -as <version>`, using the released changes as annotation
+ Push the new tag to the remote repository `git push --tags`. This should trigger the [Release](https://github.com/input-output-hk/quickcheck-dynamic/actions/workflows/release.yaml) workflow.
  * Note that it's fine to "repush" the tag and retrigger the workflow if a problem is spotted at this moment
* _(Optional)_ Retrieve the artifact attached to the workflow and upload it as a _candidate_ on [Hackage](https://hackage.haskell.org/packages/candidates/upload). This is useful to check everything's right before publishing the release
+ Publish package on [Hackage](https://hackage.haskell.org/) by manually triggering the [Publish workflow](https://github.com/input-output-hk/quickcheck-dynamic/actions/workflows/publish.yaml)
