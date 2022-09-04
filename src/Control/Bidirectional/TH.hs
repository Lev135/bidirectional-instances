{-# LANGUAGE TemplateHaskell #-}

{- | 
  Module:        Control.Bidirectional.TH
  Description:   Template haskell for generating bidirectional instances
  Copyright:     Lev Dvorkin (c) 2022
  License:       MIT
  Maintainer:    lev_135@mail.ru
  Stability:     experimental
-}
module Control.Bidirectional.TH (
    decBidirectionalInstances, 
    makeBidirectionalInstances
  ) where
import Language.Haskell.TH (Exp, Q, Dec (InstanceD, ClassD), TypeQ, DecsQ, reify, Info (ClassI), Type (ConT, AppT, TupleT), conT)
import Control.Bidirectional.Class (Bidirectional, BidirectionalRec, Constr, ConstrRec)
import Control.Monad (join)

{-| 
  Declare instance and make it bidirectional at the same time.
  Provides instances for 'Bidirectional' and 'BidirectionalRec'.

  It's suitable for declaring your own instances. To make existing instances
  (for example, from libs) bidirectional, use 'makeBidirectionalInstances'.

  You can use it for declaring multiple instances:

  > data A a = A a
  > data B a b = B a b
  > data C a b = CA a | CB b
  > 
  > decBidirectionalInstances [d| 
  >     instance Show a => Show (A a) where
  >       show (A a) = "A " ++ show a
  >     instance (Show a, Show b) => Show (B a b) where
  >       show (B a b) = "B " ++ show a ++ " " show b
  >     instance (Show a, Show b) => Show (C a b) where
  >       show (CA a) = "CA " ++ show a
  >       show (CB b) = "CB " ++ show b
  >   |] 
-} 
decBidirectionalInstances :: Q [Dec] -> Q [Dec]
decBidirectionalInstances instances = do
  inst <- instances
  (inst <>) <$> makeBidirectionalInstances instances

{- |
  Make existing instance bidirectional.
  Provides instances for 'Bidirectional' and 'BidirectionalRec'.

  It's suitable for making bidirectional existing instances, that you can't 
  change (for example, from libs). If you want to declare your one instance
  and make it bidirectional, use 'decBidirectionalInstances'.

  You can use it for declaring multiple instances:

  > makeBidirectionalInstances [d| 
  >     instance Show a => Show [a]
  >     instance (Show a, Show b) => Show (a, b)
  >     instance (Show a, Show b) => Show (Either a b)
  >   |] 
  
  Note that you need not provide the body of instance, only its head. 
-}
makeBidirectionalInstances :: Q [Dec] -> Q [Dec]
makeBidirectionalInstances instances = do
  insts <- instances
  join <$> mapM makeBidirectionalInstance insts

makeBidirectionalInstance :: Dec -> Q [Dec]
makeBidirectionalInstance inst = do
  let InstanceD Nothing constr (AppT c a) _ = inst
      c' = pure c
      a' = pure a
      constr' = pure $ mkTup constr
      constrRec' = mkTup <$> mapM mkRec constr
  [d| 
    instance (Constr $c' $a') => Bidirectional $c' $a' where
      type Constr $c' $a' = $constr'
    instance (ConstrRec $c' $a') => BidirectionalRec $c' $a' where
      type ConstrRec $c' $a' = $constrRec'
    |]
  where 
    mkTup :: [Type] -> Type
    mkTup [] = TupleT 0
    mkTup [t] = AppT (TupleT 1) t
    mkTup (t : ts) = case mkTup ts of
      AppT (TupleT n) u -> AppT (AppT (TupleT (n + 1)) t) u
      _ -> error "tmp" 
    
    mkRec :: Type -> Q Type
    mkRec (AppT a b) = [t| BidirectionalRec $(pure a) $(pure b) |]
    mkRec t = pure t
