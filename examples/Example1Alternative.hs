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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}

module Example1Alternative where

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

-- somewhere in 'bidirectional' package
class (c a, Constr c a) => Bidirectional (c :: k -> Constraint) (a :: k) where
  type Constr c a :: Constraint

-- provide instances for which we want bidirectional constraints
instance Constr Show (b, c) => Bidirectional Show (b, c) where
  type Constr Show (b, c) = (Bidirectional Show b, Bidirectional Show c)

-- then the following typechecks
instance Bidirectional Show a => Show (Term a) where
  show (Con x) = show x
  show (Tup x y) = unwords ["(", show x, ",", show y, ")"]

-- of course we can also provide Bidirectional Show instance for Term, but it 
-- isn't  necessary, unless we want to have bidirectional constraint for it
