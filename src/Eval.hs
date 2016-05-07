{-# OPTIONS_GHC -Wall #-}

module Eval where

import Ast
import Value
import Data.Maybe (fromMaybe)
import Prelude hiding (lookup)
import qualified Data.Map as M
import qualified Data.Set as S

type Exclude = S.Set EName

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

excludePatternBoundNames :: Pattern -> Exclude -> Exclude
excludePatternBoundNames pat excluded = case pat of
                                          IdPattern name -> S.insert name excluded
                                          TuplePattern pats -> foldl (\exc p -> excludePatternBoundNames p exc) excluded pats
                                          TConPattern _ pats -> foldl (\exc p -> excludePatternBoundNames p exc) excluded pats
                                          _ -> excluded

visit :: Expr -> ValueScope -> ValueEnv -> Exclude -> (ValueScope, ValueEnv, Exclude)
visit expr scope capturedEnv excluded = case expr of
                                          EList values -> foldl (\(s, c, e) value -> visit value s c e)
                                                               (scope, capturedEnv, excluded) values
                                          EIf cond thenInstrs elseInstrs -> (sco'', env'', exc'') where
                                            (sco, env, exc) = visit cond scope capturedEnv excluded
                                            (sco', env', exc') = foldl (\(s, c, e) value -> visit value s c e)
                                                                       (sco, env, exc) thenInstrs
                                            (sco'', env'', exc'') = foldl (\(s, c, e) value -> visit value s c e)
                                                                          (sco', env', exc') elseInstrs
                                          EVar name -> if name `elem` excluded
                                                      then let (scope', val) = eval expr scope
                                                           in (scope', M.insert name val capturedEnv, excluded)
                                                      else (scope, capturedEnv, excluded)
                                          EApp fn arg -> let (s, c, e) = visit fn scope capturedEnv excluded
                                                        in visit arg s c e
                                          EDestructLetBinding main _ _ -> (scope, capturedEnv, excludePatternBoundNames main excluded)
                                          EPatternMatching input cases -> let (scope', capturedEnv', excluded') = visit input scope capturedEnv excluded
                                                                         in foldl (\(s, c, e) (Case pat outcomes) -> let e' = excludePatternBoundNames pat e
                                                                                                                    in foldl (\(sco, env, exc) instr -> visit instr sco env exc)
                                                                                                                             (s, c, e') outcomes)
                                                                                  (scope', capturedEnv', excluded') cases
                                          _ -> error $ "Unhandled expr " ++ show expr


envCapturingFnWrapper :: Value -> Expr -> ValueScope -> Value
envCapturingFnWrapper fn expr scope = case expr of
                                        (ELambda params _ instrs) -> mkFn capturedEnv where
                                          excluded = foldl (\exc (Named name _) -> S.insert name exc) S.empty params
                                          capturedEnv = mkCapturedEnv excluded instrs
                                        (EDestructLetBinding (IdPattern name) args instrs) -> mkFn capturedEnv where
                                          excluded = foldl (\exc pat -> excludePatternBoundNames pat exc) (S.singleton name) args
                                          capturedEnv = mkCapturedEnv excluded instrs
                                        _ -> VUnit
                                      where
                                      mkCapturedEnv excluded instrs = let (_, capturedEnv, _) =  foldl (\(s, c, e) instr -> visit instr s c e)
                                                                                                       (scope, M.empty, excluded) instrs
                                                                      in capturedEnv
                                      mkFn capturedEnv = Fn (\arg scope' -> let scope'' = foldl (\env (k, v) -> insert k v env)
                                                                                               scope' $ M.toList capturedEnv
                                                                           in evalFn fn arg scope'')

-- to predicate if a value is match specific pattern
match :: Value -> Pattern -> ValueScope -> (ValueScope, Bool)
match input pattern scope = case pattern of
                              WildcardPattern -> (scope, True)
                              IdPattern name -> (insert name input scope, True)
                              TuplePattern pats -> case input of
                                                    VTuple items -> if length items /= length pats
                                                                   then (scope, False)
                                                                   else isAllMatch items pats
                                                    _ -> (scope, False)
                              TConPattern name pats -> case input of
                                                        Adt tag args -> if name == tag && length pats == length args
                                                                       then isAllMatch args pats
                                                                       else (scope, False)
                                                        _ -> (scope, False)
                              where
                              isAllMatch items pats = let (scope', isMatchs) = foldl (\(env, matchs) (item, pat) -> let (env', isMatch) = match item pat env
                                                                                                                   in (env', matchs ++ [isMatch]))
                                                                                     (scope, []) $ zip items pats
                                                      in (scope', all id isMatchs)

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
                    ELambda params _ instrs -> case reverse params of
                                                (Named name _):xs -> (scope, envCapturingFnWrapper fnChain expr scope) where
                                                  lastFn = chaininLastFn name instrs
                                                  fnChain = foldl (\fn (Named n _) -> chainingFn n fn) lastFn xs
                                                _ -> (scope, VUnit)
                    EApp fn arg -> case fnV of
                                    Fn f -> let (_, argV) = eval arg scope'
                                           in (scope, f argV scope')
                                    Adt _ _ -> case eval arg scope' of
                                                (_, VUnit) -> (scope, fnV)
                                                _ -> error $ "Error while evaluating " ++ show expr ++ ": " ++ show fnV ++ " constructor doesn't take arguments"
                                    _ -> error $ "Error while evaluating " ++ show expr ++ ": " ++ show fnV ++ " is not a function"
                      where
                      scope' = child scope
                      (_, fnV) = eval fn scope'
                    EIf cond thenInstrs elseInstrs -> let (_, condV) = eval cond scope
                                                     in case condV of
                                                          VBool v -> if v
                                                                    then (scope, evalInstrs thenInstrs)
                                                                    else (scope, evalInstrs elseInstrs)
                                                                    where
                                                                    evalInstrs instrs = let scope' = child scope
                                                                                        in snd $ foldl (\(env, _) instr -> eval instr env) (scope', VUnit) instrs
                                                          _ -> error $ "Error while evaluating " ++ show expr ++ ": the condition is not a boolean"
                    EPatternMatching input cases -> findPattern inputV cases
                      where (_, inputV) = eval input scope
                            findPattern :: Value -> [Case] -> (ValueScope, Value)
                            findPattern val [] = error $ "Match exception: " ++ show input ++ " = " ++ show val ++ " didn't match any case of " ++ show expr
                            findPattern val ((Case pat instrs):cs) = let (scope', isMatch) = match val pat $ child scope
                                                                     in if isMatch
                                                                        then (scope, snd $ foldl (\(env, _) instr -> eval instr env) (scope', VUnit) instrs)
                                                                        else findPattern val cs

