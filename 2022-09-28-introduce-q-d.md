# Model-Based Testing with QuickCheck

[quickcheck-dynamic](https://github.com/input-output-hk/quickcheck-dynamic) is a library jointly developed by Quviq and IOG, whose purpose is to leverage [QuickCheck](https://hackage.haskell.org/package/QuickCheck) to test stateful programs against a _Model_. In other words, it's a [_Model-Based Testing_](https://en.wikipedia.org/wiki/Model-based_testing) tool. This article wants to be a gentle introduction to the use of quickcheck-dynamic for Model-Based Testing. It describes the overall approach, how the library works, and how it's being applied within IOG to improve the reach of our testing efforts.

## Background

Testing stateful or rather effectful code using QuickCheck is not new: In particular, techniques to test _Monadic_ code with QuickCheck have been introduced in [Claessen & Hughes, 2002](https://dl.acm.org/doi/10.1145/636517.636527). `quickcheck-dynamic` is based on a state-machine approach originally implemented by Quviq in closed-source [Erlang version of QuickCheck](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.148.6554&rep=rep1&type=pdf) and put to use to test various systems as explained in John Hughes' [paper](https://publications.lib.chalmers.se/records/fulltext/232550/local_232550.pdf).

IOG already has had experience with state-machine based testing in the Consensus [Storage layer](https://github.com/input-output-hk/ouroboros-network/blob/nfrisby/old-benchmark-ledger-ops/ouroboros-consensus-test/README.md#L1) using [quickcheck-state-machine](https://hackage.haskell.org/package/quickcheck-state-machine) library, but this was not widespread practice across the various teams.

When IOG started working on the Plutus Smart Contracts language and application framework, Quviq's consultancy was sought after to help test the platform and build tools for future Smart Contract implementors. This effort lead to the development of a custom library for testing smart contracts which in turn leverages quickcheck-dynamic's state-machine model and dynamic logic language.

As quickcheck-dynamic matured and became independent of the Plutus framework, it attracted interest from other teams willing to invest into model-based testing and reuse existing effort. This finally lead to publication of the library as an independent package on [Hackage](https://hackage.haskell.org/package/quickcheck-dynamic) in the hope it will be useful to a wider audience.

## Use Cases

### Example: Thread Registry

The library comes with a complete example defining a model and reference implementation for a _Thread Registry_. It's inspired by a similar example in Erlang from a couple of papers:
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

[Hydra](https://hydra.family) is a so-called _Layer 2_ solution for Cardano that aims at increasing the throughput and latency of Cardano transactions by moving most of the traffic out of the main chain (_Layer 1_) and into smaller networks. The _Hydra Head_ protocol is described in great details in a [research paper](https://eprint.iacr.org/2020/299.pdf).

At its core, Hydra is a state machine whose transitions are Layer 1 transactions, as depicted in the following picture:

![Hydra State Machine](https://hydra.family/head-protocol/assets/images/hydra-head-lifecycle-b8449385e9041a214bf8c6e52830de3c.svg)

While the overall state machine appears relatively simple on the surface, the actual protocol is actually quite complex, involving cryptographic signatures and _Plutus Smart Contracts_ to ensure the safety of the protocol. This safety is formally expressed in the paper as _properties_ that are proven against an _Adversary Environment_ whose capabilities are well-defined.

In order to guarantee the implementation provides those safety properties, the Hydra team has developed a diversified palette of testing techniques, including the use of quickcheck-dynamic. While the careful Test-Driven Development approach taken gives reasonable confidence most use cases and issues are covered, hopes are high that such a model is able to explore more corner cases and reveal subtle issues in the protocol implementation.

What was sought after is to be able to define and test Hydra Head security properties against the real implementation. As a first example the team tackled to get a feel of how quickcheck-dynamic could used, here one of the properties from the original paper is stated:

> • Conflict-Free Liveness (Head):
>
> In presence of a network adversary, a conflict-free execution satisfies the following condition:
> For any transaction tx input via (new,tx), tx ∈ T i∈[n] Ci eventually holds.

This property and similar ones are encoded as a _Dynamic Logic_ expressions, and a suitable Model of a Hydra network is defined as an instance of `StateModel` from which test sequences representing User actions are generated.

Hydra is a distributed system where nodes are interconnected through a network layer, and each node needs to be connected to a cardano-node in order to preserve the security of the protocol. While testing an actual "cluster" of hydra and cardano nodes is definitely possible, and certainly desirable at some point in order to strengthen confidence in the whole system, it would also be very slow: Spinning up processes, establishing network connections, producing blocks on a chain, all take seconds if not minutes which makes any signficant exploration of the model state space practically infeasible.

Generated test traces are run within the [IOSim](https://github.com/input-output-hk/hydra-poc/blob/master/hydra-node/test/Hydra/ModelSpec.hs#L220) monad which allows testing 100s of traces within seconds.

Of course, this means we won't be using real TCP/IP networking stack nor connection to a real Cardano node and chain to create a Hydra network, but this is actually not a liability but an asset. By [mocking](https://abailly.github.io/posts/mocking-good-idea.html) the interfaces Hydra nodes use to communicate with other nodes and Cardano network, we are able to control the behaviour of the communication layer and _inject faults_ representing some _Adversary_: Reordering or dropping messages, tampering the data, delaying requests...

## Principles

We'll use the latter example to illustrate quickcheck-dynamic's principles and give the reader an intuition on the four steps that need to be defined in order to use it: Defining a test _Model_, stating how the model relates to the _Implementation_, expressing _Properties_ and, last but not least, checking properties.

### Defining a Model

In quickcheck-dynamic, a _Model_ is some type, a representation of the expected state of the system-under-test, for which there exists an instance of the [StateModel class](https://github.com/input-output-hk/quickcheck-dynamic/blob/abailly-iohk/blog-post/quickcheck-dynamic/src/Test/QuickCheck/StateModel.hs#L56) which sets the building blocks needed to generate and validate test sequences.

In the case of Hydra, the Model is a `WorldState` data type that control the Head parties and maintains a `GlobalState` which reflects the expected Head state:

```haskell
data WorldState = WorldState
  { hydraParties :: [(SigningKey HydraKey, CardanoSigningKey)]
  , hydraState :: GlobalState
  }
```

We won't bother the reader with details of the `GlobalState` which basically encode the states as depicted in the state-machine picture hereabove in the form of an Algebraic Data-Type.

As the old saying from Alfred Korzybski goes, "The map is not the territory", hence to be useful a _Model_ should abstract away irrelevant details for the purpose of testing. Furthermore, it's perfectly fine to use different models to test different aspects of the same implementation.

While the real Hydra layer two ledger does support a myriad of possible Cardano transactions, our model at hand is simpler and only uses _Two Party Payment_ transactions:

```haskell
data Payment = Payment
  { from :: CardanoSigningKey
  , to :: CardanoSigningKey
  , value :: Value
  }
```

The first important part of the `StateModel` instance to define is the type of `Action` that are meaningful for the given _Model_ and that can also be executed against the concrete implementation. The `Action` associated data-type is a GADT which allows the model to represent the type of _observable output_ that can be produced by the implementation and which can be part of the model's validation logic.

The Hydra model needs to represent both on-chain and off-chain actions as the properties required from Hydra relates the two. The `Action` data-type represent user-facing commands and observations that can be made on the state of the system (please note at the time of writing this, the model is incomplete):

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

Then one needs to define:
* An `initialState`,
* How to generate `arbitraryAction`s which will be used to produce sequences (or traces) of `Action`s to execute, depending on the current state,
* A `precondition` function ensuring some `Action` is valid in some state. This function may seem redundant with the generator but is actually important when _shrinking_ a failing test sequences: The execution engine will ensure the reduced sequence is still valid with respect to the model,
* A `nextState` (transition) function that evolves the model state according to the `Action`s,
* Auxiliary function `actionName` to providea  human-readable representation of actions.

The reader is invited to check the [Haddock](https://hackage.haskell.org/package/quickcheck-dynamic-1.1.0/docs/Test-QuickCheck-StateModel.html) documentation of the library for further details.

### Exercising Implementation

A _Model_ alone is somewhat useless if we don't provide a way to relate it to the actual implementation of the system-under-test. quickcheck-dynamic provides the [`RunModel`](https://hackage.haskell.org/package/quickcheck-dynamic-1.1.0/docs/Test-QuickCheck-StateModel.html#t:RunModel) typeclass for this purpose. The most important function to define is `perform` which defines how `StateModel`'s `Action` should be executed against the implementation within some monadic context `m`. Having the actual execution `Monad m` be a parameter of the `RunModel` gives more flexibility to the implementor which is not tied to `IO` for example.

In the case of Hydra, the `perform` function is defined as:

```haskell
  perform ::
    WorldState ->
    Action WorldState a ->
    LookUp (StateT (Nodes m) m ->
    StateT (Nodes m) m a
  perform st command _ = do
    case command of
      Seed{seedKeys} ->
        seedWorld seedKeys
      Commit party utxo ->
        performCommit (snd <$> hydraParties st) party utxo
...
```

The actual monad used is a classical `State` monad whose state maps a `Party` to the corresponding client connection to the Hydra node:

```haskell
data Nodes m = Nodes
  { nodes :: Map.Map Party (TestHydraNode Tx m)
  , logger :: Tracer m (HydraNodeLog Tx)
  }
```

The `m` parameter is here kept somewhat unconstrained in order to make it possible to run properties within the `IOSim` monad for faster tests execution. Also note the presence of the `logger` field which is used to capture logging output from all the nodes: Should an error happen or a postcondition fail, we can dump the log of each node which is invaluable to troubleshoot such failures. In general, testing systems in a black-box way emphasises the importance of good logging to provide as much context as possible should issues arise, and using quickcheck-dynamic makes no exception.

### Expressing Properties with Dynamic Logic

Once we have a `StateModel` we can express interesting _properties_ to check against our `RunModel`. [Dynamic Logic](https://github.com/input-output-hk/quickcheck-dynamic/blob/abailly-iohk/link-to-blog-post/quickcheck-dynamic/src/Test/QuickCheck/DynamicLogic.hs) allows one to express properties through monadic expressions relating actions, states and logic predicates.

Dynamic Logic is a form of _modal logic_, similar to [temporal logic](https://en.wikipedia.org/wiki/Temporal_logic), but whose modalities are the _actions_ (or programs) themselves. One can intertwine _programs_ and logic predicates to specify the behaviour of the former when executing some statements and actually [Dynamic Logic](https://en.wikipedia.org/wiki/Dynamic_logic_(modal_logic)) evolved from _Hoare's Triples_.

Here is the dynamic logic reformulation of the previously stated Hydra property which has been kept as close as possible to the original English statement:

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

Note that in order to define this property we have introduced two "pseudo-actions" in the _Model_, `Wait` and `ObserveConfirmedTx`: Those `Action`s have no effect on the model itself, the former being used to introduce some delay in the context of distributed and asynchronous execution, and the latter serving the purpose of _observing_ the current state of the SUT. An alternative formulation would have been to make `ObserveConfirmedTx` return the set of confirmed transactions and then express the condition as a logic predicate within the `conflictFreeLiveness` property's body.

### Checking Properties

The last step in putting quickcheck-dynamic at work is to be able to connect the _StateModel_, the _RunModel_, and the _DynamicLogic_ expression and turn those into a QuickCheck `Property` which can then be checked using the standard testing framework.

quickcheck-dynamic provides 2 functions for that purpose. The `forAllDL_` function (actually more a family of functions) will leverage `DL` formulae to generate sequences of `Action`s:

```haskell
prop_checkConflictFreeLiveness :: Property
prop_checkConflictFreeLiveness =
  forAllDL_ conflictFreeLiveness prop_HydraModel
```

The `runActions` function will execute the generated trace against the `RunModel`.

```haskell
prop_HydraModel :: Actions WorldState -> Property
prop_HydraModel actions = property $
  runIOSimProp $ do
    _ <- runActions runIt actions
    assert True
```

In this particular instance from Hydra, we need some additional machinery (the `runIOSimProp` function) to handle the execution of some monadic `PropertyM` into `IOSim` monad, turning it into a `Property`.

When run and successful, this `Property` generates the following output:

```
  check conflict-free liveness
    +++ OK, passed 100 tests.

    Actions (1334 in total):
    49.93% NewTx
    25.86% Commit
     7.50% Seed
     7.42% Init
     4.80% Abort
     2.25% ObserveConfirmedTx
     2.25% Wait

    Transitions (1334 in total):
    54.42% Open -> Open
    23.61% Initial -> Initial
     7.50% Start -> Idle
     7.42% Idle -> Initial
     4.80% Initial -> Final
     2.25% Initial -> Open
```

By default, `runActions` decorate the QuickCheck output _tabulating_ the executed `Action`. And thanks to the `monitoring` helper provided by the `RunModel`, this example also tabulates the executed _transitions_ between each of the possible values for `GlobalState`. These pieces of information are important to assess the "quality" of the model: We want to make sure its generators and the properties execution covers all interesting parts of the Model, hence exercise all relevant parts of the implementation. Please note that, as we mentioned before, the Hydra model is still a work in progress hence the reason why there's no `Open -> Final` transition!

## Conclusion

This articled introduced [quickcheck-dynamic](https://hackage.org/packages/quickcheck-dynamic), a novel Model-Based Testing library initially developed by Quviq for testing Plutus Smart Contracts and which has recently been open-sourced by IOG. I have tried to convey to the user a sense of the _Whys_, _Whats_ and _Hows_ questions this library answers through various examples and a high-level walkthrough of the steps needed to use this library for testing an implementation.

_Model-Based Testing_ is a powerful tool that simultaneously addresses both aspects of [Customer-facing tests](http://www.exampler.com/old-blog/2003/09/05.1.html#agile-testing-project-4) as Brian Marick puts it in his famous _Agile Testing Quadrant_ popularised by Lisa Crispin and Janet Gregory through their [Agile Testing](https://agiletester.ca/wp-content/uploads/sites/26/2015/07/Agile-tips-final.pdf) books: _Supporting the team_ by providing a reference model to build against, and _Critiquing the product_ through the unique state-space exploration QuickCheck provides, possibly uncovering corner cases and blind spots in either the specification or the implementation.

The library is still evolving towards better developer experience and flexibility but it's already in a state that makes it possible to test something as significant as a Hydra network. And while it may appear somewhat involved when compared to more traditional forms of writing _Functional tests_, I hope I have demonstrated quickcheck-dynamic lowers the barrier to entry associated with most MBT tools.
