{-# OPTIONS_GHC -Wall #-}

module Type where

import State
import Data.IORef
import Data.List (intercalate)
import Control.Monad (foldM, liftM)
import Data.Maybe (fromMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Text.PrettyPrint as PP
import System.IO.Unsafe (unsafePerformIO)

type Id = Int
type TName = String
type TField = String
type Types = [Type]
type TInstance = Maybe Type

data Type = TVar Id (IORef TInstance) TName -- type variable
          | TOper TName Types -- type operator
          | TRecord (M.Map TField Type)
          | TCon TName Types Type
          | TSig Type

intT :: Type
intT = TOper "Number" []

boolT :: Type
boolT = TOper "Boolean" []

charT :: Type
charT = TOper "Char" []

listT :: Type -> Type -- list type is not polymorphism
listT t = TOper "List" [t]

productT :: Types -> Type -- tuple type, product type is a name from Algebraic Data type
productT ts = TOper "*" ts

arrowT :: Type -> Type -> Type -- function type with single param
arrowT fromType toType = TOper "→" $ [fromType, toType]

functionT :: Types -> Type -> Type
functionT paramsT rtnT = foldr (\paramT resT -> arrowT paramT resT) rtnT paramsT

strT :: Type
strT = listT charT

unitT :: Type
unitT = TOper "()" []

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

stringOfType :: M.Map TName TName -> Type -> Infer String
stringOfType subrule (TVar _ inst name) = do
  instV <- readIORef inst
  case instV of
    Just inst' -> stringOfType subrule inst'
    Nothing -> return $ fromMaybe "α" $ M.lookup name subrule
stringOfType subrule (TOper name args) = case name of
                                           "*" -> do
                                             argsStr <- (intercalate " * ") <$> mapM (stringOfType subrule) args
                                             return $ "(" ++ argsStr ++ ")"
                                           "List" -> do
                                             argStr <- stringOfType subrule $ args!!0
                                             return $ "[" ++ argStr ++ "]"
                                           "→" -> do
                                             argT <- prune $ args!!0
                                             rtnT <- prune $ args!!1
                                             argStr <- stringOfType subrule argT
                                             rtnStr <- stringOfType subrule rtnT
                                             let adjust t s = case t of
                                                               TOper "→" _ -> "(" ++ s ++ ")"
                                                               _ -> s
                                             let argStr' = adjust argT argStr
                                             let rtnStr' = adjust rtnT rtnStr
                                             return $ argStr' ++ " → " ++ rtnStr'
                                           _ -> if (length args) == 0
                                               then return name
                                               else do
                                                 argsStr <- unwords <$> mapM (stringOfType subrule) args
                                                 return $ "(" ++ name ++ " " ++ argsStr ++ ")"
stringOfType subrule (TRecord pairs) = do
  pairsStr <- (intercalate ", ") <$> (mapM (\(k, v) -> ((k ++ ": ") ++) <$> stringOfType subrule v) $ M.toList pairs)
  return $ "{" ++ pairsStr ++ "}"
stringOfType subrule (TCon name types dataType) = do
  dataTypeStr <- stringOfType subrule dataType
  case types of
    [] -> return dataTypeStr
    _ -> do
      typesStr <- (intercalate ", ") <$> mapM (stringOfType subrule) types
      return $ "(" ++ name ++ " " ++ typesStr ++ " ⇒ " ++ dataTypeStr ++ ")"
stringOfType subrule (TSig t) = liftM ("typesig: " ++) $ stringOfType subrule t

getFreeVars :: Type -> Infer (S.Set TName)
getFreeVars (TVar _ inst name) = do
  instV <- readIORef inst
  case instV of
    Just inst' -> getFreeVars inst'
    Nothing -> return $ S.singleton name
getFreeVars (TOper _ args) = foldM (\acc arg -> do
                                     freeVars <- getFreeVars arg
                                     return $ S.union freeVars acc)
                                   S.empty args
getFreeVars (TRecord pairs) = foldM (\acc (_, v) -> do
                                      freeVars <- getFreeVars v
                                      return $ S.union freeVars acc)
                                    S.empty $ M.toList pairs
getFreeVars (TCon _ types dataType) = foldM (\acc t -> do
                                              freeVars <- getFreeVars t
                                              return $ S.union freeVars acc)
                                            S.empty $ types ++ [dataType]
getFreeVars (TSig t) = getFreeVars t

normalize :: Type -> Infer String
normalize t = do
  freeVars <- getFreeVars t
  let subrule = M.map (\c -> [c]) $ M.fromList $ zip (S.toList freeVars) ['α'..'ω']
  stringOfType subrule t

instance Show Type where
    showsPrec _ x = shows $ PP.text $ unsafePerformIO $ normalize x

instance Eq Type where
  TVar id1 inst1 vname1 == TVar id2 inst2 vname2 = id1 == id2 && instV1 == instV2 && vname1 == vname2 where
    instV1 = readState inst1
    instV2 = readState inst2
  TOper name1 args1 == TOper name2 args2 = name1 == name2 && args1 == args2
  TRecord pairs1 == TRecord pairs2 = pairs1 == pairs2
  TCon name1 types1 dataType1 == TCon name2 types2 dataType2 = name1 == name2 && types1 == types2 && dataType1 == dataType2
  TSig t1 == TSig t2 = t1 == t2
  _ == _ = False

instance Ord Type where
    TVar id1 inst1 vname1 <= TVar id2 inst2 vname2 = id1 <= id2 && instV1 <= instV2 && vname1 <= vname2 where
      instV1 = readState inst1
      instV2 = readState inst2
    TOper name1 args1 <= TOper name2 args2 = name1 <= name2 && args1 <= args2
    TRecord pairs1 <= TRecord pairs2 = pairs1 <= pairs2
    TCon name1 types1 dataType1 <= TCon name2 types2 dataType2 = name1 <= name2 && types1 <= types2 && dataType1 <= dataType2
    TSig t1 <= TSig t2 = t1 <= t2
    _ <= _ = False

makeVariable :: Infer Type
makeVariable = do
    i <- nextId
    name <- nextUniqueName
    instRef <- newIORef Nothing
    return $ TVar i instRef name