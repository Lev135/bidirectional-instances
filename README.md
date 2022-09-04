bidirectional-instances
===

This is a haskell package to provide wrapper class and useful template haskell
instance-generator functions for bidirectional instances, i. e. for instance
```hs
instance (A a, B b) => C (Foo a b)
```
deduce not only `forall a b. (A a, B b) => C (Foo a b)`, but also
`forall a b. C (Foo a b) => A a` and `forall a b. C (Foo a b) => B b`.
This behavior doesn't implemented in modern GHC version, but its very intuitive
(and, of course, sound) if there are no overlapping instances for `C (Foo a b)`.

There was a [paper](https://arxiv.org/pdf/1906.12242.pdf), related to this 
feature and an attempt to 
[implement it as GHC extension](https://github.com/KoenP/ghc-proposals/blob/patch-1/proposals/0000-bidirectional-instances.md).
In the discussion about alternatives for that extension this solution 
[was mentioned](https://github.com/KoenP/ghc-proposals/blob/patch-1/proposals/0000-bidirectional-instances.md#using-constraint-kinds),
but was discarded as "decidedly nontrivial, and some boilerplate is still 
required, which needs to be written for every instance".
To reduce these downsides, instances from this package are made more general
and template haskell function for generating boilerplate code are provided.
I suppose that implementation this as an compiler extension probably would be 
better for performance, but I don't have enough qualification to implement it.

Documentation
---
Detailed documentation with examples can be found in the main module 
*(Control.Bidirectional)*.


To be implemented somewhere in the future
---
I'll try to make a GHC plugin to improve this solution in two directions:
- no need for template haskell
- more intuitive syntax
More precisely, I'd like to have the following syntax for declaring 
bidirectional instances:
```hs
instance {-# BIDIRECTIONAL #-} (A a, B b) => C (Foo a b)
```
and `-XBidirectionalInstances` to make all instances without 
`OVERLAPS`/`OVERLAPPING`/`OVERLAPPABLE` bidirectional by default.

