{-# OPTIONS_GHC -Wall #-}

module Eval where

import Ast
import Value
import Prelude hiding (lookup)
import qualified Data.Map as M

-- a little non-sense here, maybe should just use VList
makeList :: [Value] -> Value
makeList res = case res of
                [] -> nil
                x:xs -> cons x $ makeList xs

eval :: Expr -> ValueScope -> (ValueScope, Value)
eval expr scope = case expr of
                    ENum v -> (scope, VNum v)
                    EBool v -> (scope, VBool v)
                    EChar v -> (scope, VChar v)
                    EStr v -> (scope, makeList $ map VChar v)
                    EUnit -> (scope, VUnit)
                    EVar name -> case lookup name scope of
                                  Just val -> (scope, val)
                                  Nothing -> error $ "Unknwon identifier " ++ show expr
                    EAccessor obj field -> case eval obj scope of
                                            (_, VRecord pairs) -> case M.lookup field pairs of
                                                              Just val -> (scope, val)
                                                              Nothing -> error $ "No field " ++ field ++ "in "++ show obj
                                            _ -> error $ "Not a record " ++ show obj
                    ETuple values -> (scope, VTuple $ map (\v -> snd (eval v scope)) values)
                    EList values -> (scope, makeList $ map (\v -> snd (eval v scope)) values)
                    ERecord pairs -> (scope, VRecord $ M.map (\v -> snd (eval v scope)) pairs)
                    ELambda params _ instrs -> undefined
