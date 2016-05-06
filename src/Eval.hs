{-# OPTIONS_GHC -Wall #-}

module Eval where

import Ast
import Value
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)
import qualified Data.Map as M

-- a little non-sense here, maybe should just use VList
makeList :: [Value] -> Value
makeList res = case res of
                [] -> nil
                x:xs -> cons x $ makeList xs

evalFn :: Value -> Value -> ValueScope -> Value
evalFn (Fn f) arg scope = f arg scope
evalFn _ _ _ = VUnit

chainingFn :: EName -> Value -> Value
chainingFn argName next = Fn (\oarg _ -> Fn (\arg scope -> let margs = case oarg of
                                                                        FnApArgs pairs -> let v = fromMaybe VUnit $ M.lookup "*" pairs
                                                                                         in FnApArgs $ M.insert "*" arg $ M.insert argName v pairs
                                                                        _ -> FnApArgs $ M.fromList [(argName, oarg), ("*", arg)]
                                                         in evalFn next margs scope))

chaininLastFn :: EName -> [Expr] -> Value
chaininLastFn argName body = Fn (\arg scope -> let scope' = case arg of
                                                            FnApArgs pairs -> foldl (\env (k, v) -> insert k v env)
                                                                                   scope
                                                                                   (M.toList $ M.insert argName (fromMaybe VUnit $ M.lookup "*" pairs) pairs)
                                                            _ -> insert argName arg scope
                                              in snd $ foldl (\(env, _) instr -> eval instr env) (scope', VUnit) body)

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
