module Control.Bidirectional (
  Bidirectional (..),
  BidirectionalRec (..),
  decBidirectionalInstances,
  makeBidirectionalInstances
)where

import Control.Bidirectional.Class (Bidirectional, BidirectionalRec)
import Control.Bidirectional.TH (makeBidirectionalInstances, decBidirectionalInstances)
