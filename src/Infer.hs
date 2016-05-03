{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TupleSections #-}

module Infer where

import Ast
import Type
import TypeScope
import State
import Data.IORef
import Text.Read (readMaybe)
import Control.Monad (when, zipWithM_, foldM, forM_)
import Control.Monad.Loops (anyM)
import qualified Data.Map as M
import qualified Data.Set as S
import Prelude hiding (lookup)

type NonGeneric = (S.Set Type)

prune :: Type -> Infer Type
prune t = case t of
            TVar _ inst _ -> do
              instV <- readIORef inst
              case instV of
                Just inst' -> do
                  newInstance <- prune inst'
                  writeIORef inst $ Just newInstance
                  return newInstance
                Nothing -> return t
            _ -> return t

occursInType :: Type -> Type -> Infer Bool
occursInType v t = do
  tP <- prune t
  case tP of
    TOper _ ts -> occursIn v ts
    v' -> return $ v == v'

occursIn :: Type -> [Type] -> Infer Bool
occursIn t = anyM (occursInType t)

isGeneric :: Type -> NonGeneric -> Infer Bool
isGeneric t nonGeneric = not <$> (occursIn t $ S.toList nonGeneric)

fresh :: Type -> NonGeneric -> Infer Type
fresh t nonGeneric = do
  mappings <- newIORef M.empty -- A mapping of TypeVariables to TypeVariables
  let freshrec ty = prune ty >>= (\tyP -> case tyP of
                                          TVar _ _ _ -> do
                                            isG <- isGeneric tyP nonGeneric
                                            if isG
                                            then do
                                              m <- readIORef mappings
                                              case M.lookup tyP m of
                                                Just tVar -> return tVar
                                                Nothing -> do
                                                  newVar <- makeVariable
                                                  modifyIORef mappings $ M.insert tyP newVar
                                                  return newVar
                                            else return tyP
                                          TOper name types -> do
                                            newTypes <- mapM freshrec types
                                            return $ TOper name newTypes
                                          TCon name types dataType -> do
                                            newTypes <- mapM freshrec types
                                            newDataType <- freshrec dataType
                                            return $ TCon newTypes newDataType
                                          TRecord valueTypes -> do
                                            newValueTypes <- foldM (\acc (k, v) -> do
                                                                    fv <- freshrec v
                                                                    return $ M.insert k fv acc)
                                                                   M.empty $ M.toList valueTypes
                                            return $ TRecord newValueTypes
                                          TExceptionCon _ _ -> return tyP)
  freshrec t

getType :: TName -> TypeScope -> NonGeneric -> Infer Type
getType name scope nonGeneric = case lookup name scope of
                                Just var -> fresh var nonGeneric
                                Nothing -> error $ "Undefined symbol " ++ name

adjustType :: Type -> Type
adjustType t = case t of
                TCon name types dataType -> functionT types dataType
                TExceptionCon name types -> functionT types exceptionT
                _ -> t

unify :: Type -> Type -> Infer ()
unify t1 t2 = do
  t1P <- prune t1
  t2P <- prune t2
  let t1PA = adjustType t1P
  let t2PA = adjustType t2P
  case (t1PA, t2PA) of
    (a@(TVar _ inst _), b) -> when (a /= b) $ do
                                 isOccurs <- occursInType a b
                                 when isOccurs $ error "Recusive unification"
                                 writeIORef inst $ Just b
    (a@(TOper _ _), b@(TVar _ _ _)) -> unify b a
    (a@(TOper name1 types1), b@(TOper name2 types2)) -> if name1 /= name2 || (length types1) /= (length types2)
                                                       then error $ "Type mismatch " ++ show a ++ " ≠ " ++ show b
                                                       else zipWithM_ unify types1 types2
    (a@(TRecord types1), b@(TRecord types2)) -> mapM (\(k, t2) -> do
                                                      case M.lookup k types1 of
                                                        Just t1 -> unify t2 t1
                                                        Nothing -> error $ "Cannot unify, no field " ++ k ++ " " ++ show a ++ ", " ++ show b)
                                                    $ M.toList types2
    _ -> error $ "Can not unify " ++ show t1 ++ ", " ++ show t2

analyze :: AstNode -> TypeScope -> NonGeneric -> Infer (Env, Type)
analyze term scope nonGeneric = case term of
                                ENum -> intT
                                EBool -> boolT
                                EChar -> charT
                                EStr -> strT
                                EUnit -> unitT
                                EList exprs -> do
                                  valueT <- makeVariable
                                  -- type checking procedure, since types of elems in a list should be the same.
                                  forM_ exprs (\e -> unify e valueT)
                                  return (scope, listT valueT)
                                ETuple exprs -> do
                                  types <- foldM (\types expr -> do
                                                    (_, ty) <- analyze expr scope nonGeneric
                                                    return $ types ++ [ty])
                                                 [] exprs
                                  return (scope, productT types)
                                ERecord pairs -> do
                                  valueTypes <- foldM (\vts (k, v) -> do
                                                        (_, t) <- analyze v scope nonGeneric
                                                        return $ M.insert k t vts)
                                                      M.empty $ M.toList pairs
                                  return (scope, TRecord valueTypes)
                                EVar name -> (scope,) <$> getType name scope nonGeneric
                                EApp fn arg -> do
                                  (_, fnT) <- analyze fn scope nonGeneric
                                  (_, argT) <- analyze arg scope nonGeneric
                                  rtnT <- makeVariable
                                  unify (functionT argT rtnT) fnT
                                  return (scope, rtnT)
                                EAccessor obj field -> do
                                  (_, objT) <- analyze obj scope nonGeneric
                                  fieldT <- makeVariable
                                  let desiredT = TRecord $ M.fromList [(field, fieldT)]
                                  unify objT desiredT
                                  return (scope, fieldT)
                                Lambda arg body -> do
                                  argT <- makeVariable
                                  (_, rtnT) <- analyze body (M.insert arg argT env) (S.insert argT nonGeneric) -- non generic on lambda arg type
                                  return $ (env, functionT argT rtnT)
                                Function fnName params body annoT -> do
                                  (types, newEnv, newNonGeneric) <- foldM (\(types', env', nonGeneric') (Param name t) ->
                                                                          case t of
                                                                            Just t' -> return (types' ++ [t'], M.insert name t' env', S.insert t' nonGeneric')
                                                                            Nothing -> do
                                                                              t' <- makeVariable
                                                                              return (types' ++ [t'], M.insert name t' env', S.insert t' nonGeneric'))
                                                                          ([], env, nonGeneric) params
                                  (_, rtnT) <- analyze body newEnv newNonGeneric
                                  let newTypes = types ++ [rtnT]
                                  case annoT of
                                    Just annoT' -> unify rtnT annoT' -- type propagation from return type to param type
                                    Nothing -> return ()
                                  let functionType = functionMT newTypes
                                  return $ (M.insert fnName functionType env, functionType)
                                Call fn args -> do
                                  types <- mapM (\arg -> snd <$> analyze arg env nonGeneric) args
                                  rtnT <- makeVariable
                                  let newTypes = types ++ [rtnT]
                                  (_, fnT) <- analyze fn env nonGeneric
                                  unify (functionMT newTypes) fnT
                                  return (env, rtnT)
                                Let n def body -> do
                                  (_, defT) <- analyze def env nonGeneric
                                  analyze body (M.insert n defT env) nonGeneric
                                LetBinding n def annoT -> do
                                  (_, defT) <- analyze def env nonGeneric
                                  case annoT of
                                    Just annoT' -> unify defT annoT' -- type checking
                                    Nothing -> return ()
                                  return (M.insert n defT env, defT)
                                LetRec n def body -> do
                                  newT <- makeVariable
                                  let newEnv = M.insert n newT env
                                  (_, defT) <- analyze def newEnv (S.insert newT nonGeneric)
                                  unify newT defT
                                  analyze body newEnv nonGeneric
