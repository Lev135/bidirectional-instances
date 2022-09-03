{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

{-
  This module contains solution for example from "Bidirectional type instances"
  by Koen Pauwels, Georgios Karachalias, Michiel Derhaeg and Tom Schrijvers
  (https://arxiv.org/pdf/1906.12242.pdf), using this package
-}

module Example1TH where

import Data.Kind (Constraint, Type)
import Control.Bidirectional ( Bidirectional, makeBidirectionalInstances ) 
import Control.Bidirectional.Class (BidirectionalRec)

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

-- provide instances for which we want bidirectional constraints
makeBidirectionalInstances [d|
    instance (Show b, Show c)  => Show (b, c)
  |]

-- then the following typechecks
instance BidirectionalRec Show a => Show (Term a) where
  show (Con x) = show x
  show (Tup x y) = unwords ["(", show x, ",", show y, ")"]

-- of course we can also provide Bidirectional Show instance for Term, but it 
-- isn't  necessary, unless we want to have bidirectional constraint for it
