{-
  This module contains solution for example from "Bidirectional type instances"
  by Koen Pauwels, Georgios Karachalias, Michiel Derhaeg and Tom Schrijvers
  (https://arxiv.org/pdf/1906.12242.pdf), alternative to provided in
  [Example1](./Example1.hs). It's useful if in we have no access to the class
  definition, but want to use bidirectional constraint
-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Example1 where

import Data.Kind (Constraint, Type)

data Term :: Type -> Type where
  Con :: a -> Term a
  Tup :: Term b -> Term c -> Term (b, c)

{-
-- Fails to type check:
instance Show a => Show (Term a) where
  show (Con x) = show x
  show (Tup x y) = unwords ["(", show x, ",", show y, ")"]
-}

{-
  Some kind of solution using modern haskell without new extensions
-}

-- make a wrapper over standard Show class from Prelude
class (Show a, ShowC a) => Show' a where
  type ShowC a :: Constraint

-- provide instances for which we want bidirectional constraints
instance (Show' b, Show' c) => Show' (b, c) where
  type ShowC (b, c) = (Show' b, Show' c)

-- then the following typechecks
instance Show' a => Show (Term a) where
  show (Con x) = show x
  show (Tup x y) = unwords ["(", show x, ",", show y, ")"]

-- we can also provide Show' instance for Term, but it isn't necessary,
-- unless we want to have bidirectional constraint for it
