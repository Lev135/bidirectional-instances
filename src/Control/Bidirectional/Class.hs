{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Control.Bidirectional.Class where
import Data.Kind (Constraint)

class (c a, Constr c a) => Bidirectional (c :: k -> Constraint) (a :: k) where
  type Constr c a :: Constraint
