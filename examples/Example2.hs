{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Example2 where

import Data.Kind (Type)

data Nat :: Type where
  Z :: Nat
  S :: Nat -> Nat

data Vec :: Nat -> Type -> Type where
  VN :: Vec Z a
  VC :: a -> Vec n a -> Vec (S n) a

append :: Vec n a -> Vec m a -> Vec (Add n m) a
append VN ys = ys
append (VC x xs) ys = VC x (append xs ys)

type family Add n m where
  Add Z m = m
  Add (S n) m = S (Add n m)
