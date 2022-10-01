# Model-Based Testing with QuickCheck

[quickcheck-dynamic](https://github.com/input-output-hk/quickcheck-dynamic) is a library jointly developed by Quviq and IOG, whose purpose is to leverage [QuickCheck](https://hackage.haskell.org/package/QuickCheck) to test stateful programs against a _Model_. In other words, it's a [_Model-Based Testing_](https://en.wikipedia.org/wiki/Model-based_testing) tool. This article describes the approach, the library, and how it's being applied within IOG to improve the reach of our testing efforts.

## Background

Testing stateful or rather effectful code using QuickCheck is not new: In particular, techniques to test _Monadic_ code with QuickCheck have been introduced in [Claessen & Hughes, 2002](https://dl.acm.org/doi/10.1145/636517.636527). `quickcheck-dynamic` is based on a state-machine approach originally implemented by Quviq in closed-source Erlang version of QuickCheck and put to use to test various systems as explained in John Hughes' [paper](https://publications.lib.chalmers.se/records/fulltext/232550/local_232550.pdf).

IOG already has had experience with state-machine based testing in the Consensus [Storage layer](https://github.com/input-output-hk/ouroboros-network/blob/nfrisby/old-benchmark-ledger-ops/ouroboros-consensus-test/README.md#L1) using [quickcheck-state-machine](https://hackage.haskell.org/package/quickcheck-state-machine) library, but this was not widespread practice across the various teams.

When IOG started working on the Plutus Smart Contracts language and application framework, Quviq's consultancy was sought after to help test the platform and build tools for future Smart Contract implementors. This effort lead to the development of a custom library for testing smart contracts which in turn leverages quickcheck-dynamic's state-machine model and dynamic logic language.

As quickcheck-dynamic matured and became independent of the Plutus framework, it attracted interest from other teams willing to invest into model-based testing and reuse existing effort. This finally lead to publication of the library as an independent package on [Hackage](https://hackage.haskell.org/package/quickcheck-dynamic) in the hope it will be useful to a wider audience.

In the rest of the article, we illustrate the usefulness of the library on several use cases, then proceed to explain its principles and how to

## Use Cases

### Example: Thread Registry

The library comes with a complete example defining a model and reference implementation for a _Thread Registry_. It's inspired by a similar example in Erlag from a couple of papers:
  * [How well are your requirements tested?](https://publications.lib.chalmers.se/records/fulltext/232552/local_232552.pdf)
  * and [Understanding Formal Specifications through Good Examples](https://mengwangoxf.github.io/Papers/Erlang18.pdf)

The tests here use IOG's concurrent execution simulator library [io-sim](https://github.com/input-output-hk/io-sim) to speed-up testing.

### Lockstep Testing

Edsko de Vries wrote a [nice blog post](https://well-typed.com/blog/2022/09/lockstep-with-quickcheck-dynamic/) to compare `quickcheck-dynamic` with [quickcheck-state-machine](https://hackage.haskell.org/package/quickcheck-state-machine), another library to write model-based tests on top of QuickCheck. This blog post introduces [quickcheck-lockstep](https://github.com/well-typed/quickcheck-lockstep) which provides _lockstep-style_ testing on top of quickcheck-dynamic.

Lockstep-style testing is a special case of Model-Based Testing whereby what's tested at each execution step of a test sequence is the equivalence up to some observability function, of the return values expected by the _Model_ and the one provided by the _Implementation_. In other words, if we consider each step in the state-machine as a transition that, given some input and a starting state, produces some output and possibly a new state, then lockstep-style testing checks equivalence of the _output traces_ from the model and the implementation.

The quickcheck-lockstep library provides generic implementations for most of the methods of the `StateModel` typeclass and dedicated type-classes to relate the Model and the Implementation.

### Plutus Contracts

Within IOG, the quickcheck-dynamic testing approach was initially applied to provide a testing framework for Smart Contracts developers within the Plutus Application Backend. The Plutus documentation contains a [detailed tutorial](https://plutus-apps.readthedocs.io/en/latest/plutus/tutorials/contract-models.html) on how to model smart contracts and tests them using underlying _Emulator_.

While the _Contract Model_ is a specialised library dedicated to Smart contracts modeling and testing, the underlying principles are similar:
* Define a `ContractModel` with some state and actions representing the system behaviour,
* Define a `perform`  function that describes how the model's actions translate to real world calls to a Contract's _endpoints_,
* then test the contracts implementation using properties written in the _Dynamic Logic_ language provided by the framework.

On top of quickcheck-dynamic, the _Contract Model_ provides machinery to simplify definition of a model in the case of Plutus smart contracts and running tests against a set of _Wallets. More importantly, it predefines critical properties that all Smart Contracts should validate, like the [No Locked Funds](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-contract/src/Plutus/Contract/Test/ContractModel/Internal.hs#L1719) or the more subtle [Double Satisfaction](https://github.com/input-output-hk/plutus-apps/blob/main/plutus-contract/src/Plutus/Contract/Test/ContractModel/DoubleSatisfaction.hs) property. Smart contracts are somewhat infamous for being subject to subtle coding errors leading into loopholes which attackers can abuse to steal currencies from innocent users, because of the intrisic complexity of programming in a highly distributed and asynchronous model.

The Model-Based Testing approach supported by quickcheck-dynamic gives developers the tools to explore the state space in a much more systematic way.

### Hydra

#### Overview

[Hydra](https://hydra.family) is a so-called _Layer 2_ solution for Cardano that aims at increasing the throughput and latency of Cardano transactions by moving most of the traffic out of the main chain (_Layer 1_) and into smaller networks. The _Hydra Head_ protocol is described in great details in a [research paper](https://eprint.iacr.org/2020/299.pdf).

At its core, Hydra is a state machine whose transitions are Layer 1 transactions, as depicted in the following picture:

![Hydra State Machine](https://hydra.family/head-protocol/assets/images/hydra-head-lifecycle-b8449385e9041a214bf8c6e52830de3c.svg)

While the overall state machine appears relatively simple on the surface, the actual protocol is actually quite complex, involving cryptographic signatures and _Plutus Smart Contracts_ to ensure the safety of the protocol. This safety is formally expressed in the paper as _properties_ that are proven against an _Adversary Environment_ whose capabilities are well-defined.

In order to guarantee the implementation provides those safety properties, the Hydra team has developed a diversified palette of testing techniques, including the use of quickcheck-dynamic. While the careful Test-Driven Development approach taken gives reasonable confidence most use cases and issues are covered, hopes are high that such a model is able to explore more corner cases and reveal subtle issues in the protocol implementation.

What is actually wanted is to be able to define and test Hydra Head security properties against the real implementation. As a first example, here is stated one of the properties from the original paper:

> • Conflict-Free Liveness (Head):
>
> In presence of a network adversary, a conflict-free execution satisfies the following condition:
> For any transaction tx input via (new,tx), tx ∈ T i∈[n] Ci eventually holds.

This property can be restated in an excutable form using quickcheck-dynamic's [Dynamic Logic](https://github.com/input-output-hk/quickcheck-dynamic/blob/abailly-iohk/link-to-blog-post/quickcheck-dynamic/src/Test/QuickCheck/DynamicLogic.hs) language:

```haskell
conflictFreeLiveness :: DL WorldState ()
conflictFreeLiveness = do
  anyActions_
  getModelStateDL >>= \case
    st@WorldState{hydraState = Open{}} -> do
      (party, payment) <- forAllQ (nonConflictingTx st)
      action $ Model.NewTx party payment
      eventually (ObserveConfirmedTx payment)
    _ -> pass
 where
  nonConflictingTx st = withGenQ (genPayment st) (const [])
  eventually a = action (Wait 10) >> action a
```

#### Implementation

The current model is relatively simple and covers the basics of the protocol. One key aspect of quickcheck-dynamic's approach is that the model need not be exactly as detailed as the implementation. In the case of Hydra, the model does not detail the myriad possible variations of transactions Cardano permits and only uses _Two Party Payment_ transactions:

```haskell
data Payment = Payment
  { from :: CardanoSigningKey
  , to :: CardanoSigningKey
  , value :: Value
  }
```

The Model's state is a `WorldState` data type that control the Head parties and maintains a `GlobalState` which reflects the expected Head state:

```haskell
data WorldState = WorldState
  { hydraParties :: [(SigningKey HydraKey, CardanoSigningKey)]
  , hydraState :: GlobalState
  }
```

Furthermore, the model needs to represent both on-chain and off-chain actions as the properties required from Hydra relates the two. There is an `instance StateModel WorldState` whose `Action` represent user-facing commands and observations that can be made on the state of the system (please note at the time of writing this, the model is incomplete):

```haskell
  data Action WorldState a where
    Seed :: {seedKeys :: [(SigningKey HydraKey, CardanoSigningKey)]} -> Action WorldState ()

    Init :: Party -> ContestationPeriod -> Action WorldState ()

    Commit :: Party -> UTxOType Payment -> Action WorldState ActualCommitted

    Abort :: Party -> Action WorldState ()

    NewTx :: Party -> Payment -> Action WorldState ()

    Wait :: DiffTime -> Action WorldState ()

    ObserveConfirmedTx :: Payment -> Action WorldState ()
```

As expected, the rest of the [StateModel](https://github.com/input-output-hk/hydra-poc/blob/master/hydra-node/test/Hydra/Model.hs#L207) instance's code defines how sequence of actions are generated, what are the `precondition` and `postcondition` for each action, and the state transition function for each `Action`.

The second half of the work needed to use quickcheck-dynamic is to define a `RunModel` whose purpose is to run the action sequences generated by the `StateModel` against the actual implementation. Hydra is a distributed system where nodes are interconnected through a network layer, and each node needs to be connected to a cardano-node in order to preserve the security of the protocol. While testing an actual "cluster" of hydra and cardano nodes is definitely possible, and certainly desirable at some point in order to strengthen confidence in the whole system, it would also be very slow: Spinning up processes, establishing network connections, producing blocks on a chain, all take seconds if not minutes which makes any signficant exploration of the model state space practically infeasible.

The Hydra `RunModel` is defined in terms of an abstract `Monad m` with some constraints from the [io-sim](https://github.com/input-output-hk/io-sim) typeclasses. And the actual tests are run within the [IOSim](https://github.com/input-output-hk/hydra-poc/blob/master/hydra-node/test/Hydra/ModelSpec.hs#L220) monad, an in-process concurrent execution simulator developped within IOG to test the networking layer. This allows 100s of test sequences to be run within seconds.

Of course, this means we won't be using real TCP/IP networking stack nor connection to a real Cardano node and chain to create a Hydra network, but this is actually not a liability but an asset. By [mocking](https://abailly.github.io/posts/mocking-good-idea.html) the interfaces Hydra nodes use to communicate with other nodes and Cardano network, we are able to control the behaviour of the communication layer and _inject faults_ representing some _Adversary_: Reordering or dropping messages, tampering the data, delaying requests...


## Principles

### Defining a Model

### Exercising Implementation

why separating from model, performing stuff, IOSim

### Expressing Properties with Dynamic Logic
