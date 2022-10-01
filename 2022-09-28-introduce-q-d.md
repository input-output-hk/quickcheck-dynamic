# Model-Based Testing with QuickCheck

## Background

* Quviq and John Hughes original work on SM based testing
* Quviq hired by  IOG to help build solid testing framework for Plutus
  * Note that q-s-m was already used within IOG for Network testing
* MBT attracted interest from other teams which lead to ttry to reuse (eg. Hydra)
* Finally open-sourced in August

## Use Cases

### Hydra

#### Overview

[Hydra](https://hydra.family) is a so-called _Layer 2_ solution for Cardano that aims at increasing the throughput and latency of Cardano transactions by moving most of the traffic out of the main chain (_Layer 1_) and into smaller networks. The _Hydra Head_ protocol is described in great details in a [research paper](https://eprint.iacr.org/2020/299.pdf).

At its core, Hydra is a state machine whose transitions are Layer 1 transactions, as depicted in the following picture:

![Hydra State Machine](https://hydra.family/head-protocol/assets/images/hydra-head-lifecycle-b8449385e9041a214bf8c6e52830de3c.svg)

While the overall state machine appears relatively simple on the surface, the actual protocol is actually quite complex, involving cryptographic signatures and _Plutus Smart Contracts_ to ensure the safety of the protocol. This safety is formally expressed in the paper as _properties_ that are proven against an _Adversary Environment_ whose capabilities are well-defined.

In order to guarantee the implementation provides those safety properties, the Hydra team has developed a diversified palette of testing techniques, including the use of quickcheck-dynamic. While the careful Test-Driven Development approach taken gives reasonable confidence most use cases and issues are covered, hopes are high that such a model is able to explore more corner cases and reveal subtle issues in the protocol implementation.

What is actually wanted is to be able to define and test Hydra Head security properties against the real implementation.

As a first example, here is stated one of the properties from the original paper:

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

The Model state is a `WorldState` data type that control the Head parties and maintains a `GlobalState` which reflects the expected Head state:

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

The Hydra `RunModel` is defined in terms of an abstract `Monad m` with some constraints from the [io-sim](https://github.com/input-output-hk/io-sim) typeclasses. And the actual tests are run within the [IOSim](https://github.com/input-output-hk/hydra-poc/blob/master/hydra-node/test/Hydra/ModelSpec.hs#L220) monad, an in-process I/O simulator developped within IOG to test the networking layer.

### Example: Thread Registry

### Plutus Contracts

### Lockstep Testing

## Principles

### Defining a Model

### Exercising Implementation

why separating from model, performing stuff, IOSim

### Expressing Properties with Dynamic Logic
