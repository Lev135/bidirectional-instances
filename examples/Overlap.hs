{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Overlap where

import Control.Bidirectional (decBidirectionalInstances)

data A a = A a

decBidirectionalInstances [d| 
    instance Show a => Show (A a) where
      show (A a) = "A " ++ show a
  |]

instance {-# OVERLAPS #-} Show (A Int) where
  show (A a) = "Integral A: " ++ show (toInteger a)
