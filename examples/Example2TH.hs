{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Example2TH where

import Control.Monad.State (MonadState, StateT)
import Control.Monad.Except (ExceptT)
import Data.Kind (Constraint)
import Control.Bidirectional (makeBidirectionalInstances, Bidirectional)


class (forall s e m e'. MonadState s (ExceptT e m) => 
  MonadState s (ExceptT e' m)) => W

{-
-- this instance isn't typechecked
instance W
-}

makeBidirectionalInstances [d|
    instance Monad m => MonadState s (StateT s m)
    instance MonadState s m => MonadState s (ExceptT e m)
  |]

class (forall s e m e'. Bidirectional (MonadState s) (ExceptT e m) => 
  Bidirectional (MonadState s) (ExceptT e' m)) => W'

-- but this instance is correct
instance W'

