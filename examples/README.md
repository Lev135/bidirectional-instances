This folder contains solution for examples from "Bidirectional type instances"
by Koen Pauwels, Georgios Karachalias, Michiel Derhaeg and Tom Schrijvers
(https://arxiv.org/pdf/1906.12242.pdf)
by means of type families without any new extension.

However, solution for the first example seems too boilerplate. 
It maybe reasonable to realize some extension making this code
```haskell
class {-# BIDIRECTIONAL #-} Show a where
  show :: a -> String

instance (Show b, Show c) => Show (b, c) where
  show (x, y) = unwords ["(", show x, ",", show y, ")"]

instance Show a => Show (Term a) where
  show (Con x) = show x
  show (Tup x y) = unwords ["(", show x, ",", show y, ")"]
```
behave as the following from [the first example](Example1.hs)
```haskell
class ShowC a => Show' a where
  type ShowC a :: Constraint
  show' :: a -> String

instance (Show' b, Show' c) => Show' (b, c) where
  type ShowC (b, c) = (Show' b, Show' c)
  show' (x, y) = unwords ["(", show' x, ",", show' y, ")"]

instance Show' a => Show' (Term a) where
  type ShowC (Term a) = Show' a
  show' (Con x) = show' x
  show' (Tup x y) = unwords ["(", show' x, ",", show' y, ")"]
```

Maybe for more complex classes this transformation can be more complex or even
impossible. I'd be glad if someone provides them. I also wonder if it there are
some disadvantages for bidirectional approach compared with unidirectional.
As far as I can see, there will be problems only for overlapping instances.
However, what will be more useful for simple type classes like `Show`: to have
backward constraint or make it possible to overlap instances? Are there any 
other drawbacks of this changes (maybe efficiency)?

It's also interesting for me, if there were conversations on this topic or even 
attempts to realize it. Anyway, I'll be happy to discuss it
