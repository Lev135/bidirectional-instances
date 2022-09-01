{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Example2 where
import Control.Monad.State (MonadState, StateT)
import Control.Monad.Except (ExceptT)
import Data.Kind (Constraint)


class (forall s e m e'. MonadState s (ExceptT e m) => 
  MonadState s (ExceptT e' m)) => W

{-
-- this instance isn't typechecked
instance W
-}

class (MonadState s m, MonadStateC s m) => MonadState' s m where
  type MonadStateC s m :: Constraint

instance Monad m => MonadState' s (StateT s m) where
  type MonadStateC s (StateT s m) = Monad m

instance MonadState' s m => MonadState' s (ExceptT e m) where
  type MonadStateC s (ExceptT e m) = MonadState' s m

class (forall s e m e'. MonadState' s (ExceptT e m) => 
  MonadState' s (ExceptT e' m)) => W'

-- but this instance is correct
instance W'
