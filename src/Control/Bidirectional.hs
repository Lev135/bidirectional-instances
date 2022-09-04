{- | 
  Module:        Control.Bidirectional
  Description:   All you need to use bidirectional instances
  Copyright:     Lev Dvorkin (c) 2022
  License:       MIT
  Maintainer:    lev_135@mail.ru
  Stability:     experimental

  This module provides class wrapper for instances that should be bidirectional,
  i. e. lets GHC know that from instance

  > instance (A a, B b) => C (Foo a b)

  not only @forall a b. (A a, B b) => C (Foo a b)@, but also
  @forall a b. C (Foo a b) => A a@ and @forall a b. C (Foo a b) => B b@
  can be deduced. This is correct, provided that there are no overlapping 
  instances for @C (Foo a b)@.

  === __Example 1: showing @GADT@__
  Suppose we want to write @Show@ instance for the following GADT type:

  > data Term :: Type -> Type where
  >   Con :: a -> Term a
  >   Tup :: Term b -> Term c -> Term (b, c)

  It's a bit tricky, because an obvious declaration

  > instance Show a => Show (Term a) where
  >   show (Con x) = show x
  >   show (Tup x y) = unwords ["(", show x, ",", show y, ")"]

  fails to typecheck, because in the second equation we apply @show@ function
  to @x@, so we need @Show@ instance for @b@ (from GADT constructor).
  But we only have @Show a@ where @a ~ (b, c)@. So we need to deduce
  @Show (b, c) => Show b@ and GHC fails to do this. This package provides 
  a solution for this problem:

  First of all, we need to make instance @Show (b, c)@ bidirectional.
  Maybe sometime in the future it will be declared in @Prelude@, but now we
  need to make it manually. TH functions from this module can help to reduce
  boilerplate:

  > makeBidirectionalInstances [d|
  >     instance (Show b, Show c)  => Show (b, c)
  >   |]

  After bidirectional instance has been declared, we can use our previous
  @Show (Term a)@ declaration with a small change: changing constraint
  from @Show a@ to @BidirectionalRec Show a@.

  > instance BidirectionalRec Show a => Show (Term a) where
  >   show (Con x) = show x
  >   show (Tup x y) = unwords ["(", show x, ",", show y, ")"]

  Why we need 'BidirectionalRec' constraint, but not simple 'Bidirectional'?
  It's so because we may have nested tuples: @(Tup (Tup x y) z)@ and for showing
  @(Tup x y)@ we also need bidirectional @Show@ instance. So GHC must infer
  @BidirectionalRec Show (b, c) => BidirectionalRec Show b@ and
  @Bidirectional Show (b, c) => Show b@ is not enough.

  === __Example 2: mapping error type for @ErrorT@ preserving MonadState constraint__
  Suppose we want to change @e@ type for @ExceptT@ transformer preserving 
  knowledge of @MonadState s@ instance for composed monad. So we want to infer
  @MonadState s (ExceptT e m) => MonadState s (ExceptT e' m)@.
  Using bidirectional instances it can be done this way:
  
  > Bidirectional (MonadState s) (ExceptT e m) 
  >   => MonadState s ExceptT e m 
  >   => Bidirectional (MonadSTate s) EXceptT e m

  Thus, the following is well-typed:

  > makeBidirectionalInstances [d|
  >   instance Monad m => MonadState s (StateT s m)
  >   instance MonadState s m => MonadState s (ExceptT e m)
  > |]
  > 
  > class (forall s e m e'. Bidirectional (MonadState s) (ExceptT e m) => 
  >   Bidirectional (MonadState s) (ExceptT e' m)) => W'
  > instance W'

  == Interaction with overlapping instances
  
  As was mentioned above backward implication is sound only when we have 
  no overlapping instances for.
  However, solution from this package /can/ work with overlapping instances,
  provided that only one of them is selected to use in backward direction.
  Selected instance should be passed to 
  `makeBidirectionalInstances`/`decBidirectionalInstances`.

  For example this code is correct:

  > data A a = A a
  > 
  > decBidirectionalInstances [d| 
  >     instance Show a => Show (A a) where
  >       show (A a) = "A " ++ show a
  >   |]
  > 
  > instance {-# OVERLAPS #-} Show (A Int) where
  >   show (A a) = "Integral A: " ++ show (toInteger a)
-}
module Control.Bidirectional (
  Bidirectional (..),
  BidirectionalRec (..),
  decBidirectionalInstances,
  makeBidirectionalInstances
) where

import Control.Bidirectional.Class (Bidirectional(..), BidirectionalRec(..))
import Control.Bidirectional.TH (makeBidirectionalInstances, decBidirectionalInstances)
