{-# LANGUAGE TemplateHaskell #-}
module Control.Bidirectional.TH where
import Language.Haskell.TH (Exp, Q, Dec (InstanceD, ClassD), TypeQ, DecsQ, reify, Info (ClassI), Type (ConT, AppT, TupleT), conT)
import Control.Bidirectional.Class (Bidirectional, Constr)

makeBidirectionalInstances :: Q [Dec] -> Q [Dec]
makeBidirectionalInstances instances = do
  insts <- instances
  mapM makeBidirectionalInstance insts


makeBidirectionalInstance :: Dec -> Q Dec
makeBidirectionalInstance inst = do
  let InstanceD Nothing constr (AppT c a) _ = inst
      c' = pure c
      a' = pure a
      constr' = pure $ mkTup constr
  head <$> [d| 
    instance (Constr $c' $a') => Bidirectional $c' $a' where
      type Constr $c' $a' = $constr'
    |]
  where 
    mkTup :: [Type] -> Type
    mkTup [] = TupleT 0
    mkTup [t] = AppT (TupleT 1) t
    mkTup (t : ts) = case mkTup ts of
      AppT (TupleT n) u -> AppT (AppT (TupleT (n + 1)) t) u
      _ -> error "tmp" 
