{- | 
  Module:        Control.Bidirectional
  Description:   All you need to use bidirectional instances
  Copyright:     Lev Dvorkin (c) 2022
  License:       MIT
  Maintainer:    lev_135@mail.ru
  Stability:     experimental
-}
module Control.Bidirectional (
  Bidirectional (..),
  BidirectionalRec (..),
  decBidirectionalInstances,
  makeBidirectionalInstances
) where

import Control.Bidirectional.Class (Bidirectional, BidirectionalRec)
import Control.Bidirectional.TH (makeBidirectionalInstances, decBidirectionalInstances)
