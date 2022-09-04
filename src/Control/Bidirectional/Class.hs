{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{- | 
  Module:        Control.Bidirectional.Class
  Description:   Base types for bidirectional instances
  Copyright:     Lev Dvorkin (c) 2022
  License:       MIT
  Maintainer:    sample@email.com
  Stability:     experimental
-}
module Control.Bidirectional.Class (
    Bidirectional (..), 
    BidirectionalRec (..)
  ) where

import Data.Kind (Constraint)

{-| 
  Class for non-recursive bidirectional instances, i. e. for instances,
  such that their components constraints ('Constr') is ordinary instance.
 
  Arguments:
    
  - @c@ class for which we declare bidirectional instance
  - @a@ data type for which instance is provided

  For example:

  > instance Show a => Bidirectional Show [a] where
  >   type ConstrRec Show [a] = Show a
 
  is correct 'Bidirectional' instance. If you want to have bidirectional
  'Show' instance in backward constraint, use 'BidirectionalRec'
  
  Instances for this class are supposed to be generated by 
  'Control.Bidirectional.makeBidirectionalInstances` or by
  'Control.Bidirectional.decBidirectionalInstances`.
-}
class (c a, Constr c a) => Bidirectional (c :: k -> Constraint) (a :: k) where
  -- | Constraint for backwards inference. 
  -- Should not be recursively bidirectional (it means that all constraints 
  -- should not be wrapped in 'Bidirectional', e. g. @Show a@ but not
  -- @Bidirectional Show a@)
  type Constr c a :: Constraint

{-| 
  Class for recursive bidirectional instances, i. e. for instances, such
  that components also have bidirectional instance. Use 'Bidirectional'
  non-recursive variant, if you need only one step in backward direction.

  Arguments:
    
  - @c@ class for which we declare bidirectional instance
  - @a@ data type for which instance is provided

  For example, this is a nice recursive instance:
  
  > instance BidirectionalRec Show a => BidirectionalRec Show [a] where
  >   type ConstrRec Show [a] = BidirectionalRec Show a
  
  but this one isn't (actually it should be a 'Bidirectional' instance):
  
  > instance Show a => BidirectionalRec Show [a] where
  >   type ConstrRec Show [a] = Show a

  Instances for this class are supposed to be generated by 
  'Control.Bidirectional.makeBidirectionalInstances` or by
  'Control.Bidirectional.decBidirectionalInstances`.
-}
class (c a, ConstrRec c a, Bidirectional c a) 
  => BidirectionalRec (c :: k -> Constraint) (a :: k) where
  -- | Constraint for backwards inference. 
  -- Should be recursively bidirectional (it means that all constraints should
  -- be wrapped in 'BidirectionalRec', e. g. @BidirectionalRec Show a@ but not
  -- simply @Show a@)
  type ConstrRec c a :: Constraint
