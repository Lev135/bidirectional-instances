{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Control.Bidirectional

-- makeBidirectionalInstance ''Show [t| (a, b) |] [t| (Show a, Show b) |]

makeBidirectionalInstances [d| 
    instance (Show a, Show b) => Show (a, b)
  |]

main :: IO ()
main = putStrLn "Hello, Haskell!"
