{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}

module Example2Alternative where
import Control.Monad.State (MonadState, StateT)
import Control.Monad.Except (ExceptT)
import Data.Kind (Constraint)


class (forall s e m e'. MonadState s (ExceptT e m) => 
  MonadState s (ExceptT e' m)) => W

{-
-- this instance isn't typechecked
instance W
-}

-- somewhere in 'bidirectional' package
class (c a, Constr c a) => Bidirectional (c :: k -> Constraint) (a :: k) where
  type Constr c a :: Constraint

instance Constr (MonadState s) (StateT s m) => Bidirectional (MonadState s) (StateT s m) where
  type Constr (MonadState s) (StateT s m) = Monad m

instance Constr (MonadState s) (ExceptT s m) => Bidirectional (MonadState s) (ExceptT e m) where
  type Constr (MonadState s) (ExceptT e m) = Bidirectional (MonadState s) m

class (forall s e m e'. Bidirectional (MonadState s) (ExceptT e m) => 
  Bidirectional (MonadState s) (ExceptT e' m)) => W'

-- but this instance is correct
instance W'
