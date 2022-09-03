{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Example1PreludeTH where

import Control.Bidirectional (decBidirectionalInstances, BidirectionalRec)
import Data.Kind (Type)

data Term :: Type -> Type where
  Con :: a -> Term a
  Tup :: Term b -> Term c -> Term (b, c)

class Show' a where
  show' :: a -> String

decBidirectionalInstances [d|
    instance (Show' a, Show' b) => Show' (a, b) where
      show' (x, y) = unwords ["(", show' x, ",", show' y, ")"]
  |]

instance BidirectionalRec Show' a => Show' (Term a) where
  show' (Con x) = show' x
  show' (Tup x y) = unwords ["(", show' x, ",", show' y, ")"]
