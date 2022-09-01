{-
  This module contains the first example from "Bidirectional type instances"
  by Koen Pauwels, Georgios Karachalias, Michiel Derhaeg and Tom Schrijvers
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

-- somewhere in Prelude'
class ShowC a => Show' a where
  type ShowC a :: Constraint
  show' :: a -> String

-- also in Prelude'
instance (Show' b, Show' c) => Show' (b, c) where
  type ShowC (b, c) = (Show' b, Show' c)
  show' (x, y) = unwords ["(", show' x, ",", show' y, ")"]

-- then the following typechecks
instance Show' a => Show' (Term a) where
  type ShowC (Term a) = Show' a
  show' (Con x) = show' x
  show' (Tup x y) = unwords ["(", show' x, ",", show' y, ")"]
