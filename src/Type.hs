{-# OPTIONS_GHC -Wall #-}

module Type where

import State
import Data.IORef
import Data.List(intercalate)
import qualified Data.Map as M
import qualified Text.PrettyPrint as PP
import System.IO.Unsafe(unsafePerformIO)

type Id = Int
type TName = String
type TField = String
type Types = [Type]
type TInstance = Maybe Type

data Type = TVar Id (IORef TInstance) TName -- type variable
          | TOper TName Types -- type operator
          | TRecord (M.Map TField Type)
          | TCon TName Types Type

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

stringOfType :: Type -> Infer String
stringOfType (TVar _ inst name) = do
  instV <- readIORef inst
  case instV of
    Just inst' -> stringOfType inst'
    Nothing -> return name
stringOfType (TOper name args) = case name of
                                  "*" -> do
                                    argsStr <- (intercalate " * ") <$> mapM stringOfType args
                                    return $ "(" ++ argsStr ++ ")"
                                  "List" -> do
                                    argStr <- stringOfType $ args!!0
                                    return $ "[" ++ argStr ++ "]"
                                  "→" -> do
                                    let argT = args!!0
                                    let rtnT = args!!1
                                    argStr <- stringOfType argT
                                    rtnStr <- stringOfType rtnT
                                    let adjust t s = case t of
                                                      TOper "→" _ -> "(" ++ s ++ ")"
                                                      _ -> s
                                    let argStr' = adjust argT argStr
                                    let rtnStr' = adjust rtnT rtnStr
                                    return $ argStr' ++ " → " ++ rtnStr'
                                  _ -> if (length args) == 0
                                      then return name
                                      else do
                                        argsStr <- unwords <$> mapM stringOfType args
                                        return $ "(" ++ name ++ ":" ++ argsStr ++ ")"
stringOfType (TRecord pairs) = do
  pairsStr <- (intercalate ", ") <$> (mapM (\(k, v) -> (k ++) <$> stringOfType v) $ M.toList pairs)
  return $ "{" ++ pairsStr ++ "}"
stringOfType (TCon name types dataType) = do
  dataTypeStr <- stringOfType dataType
  case types of
    [] -> return dataTypeStr
    _ -> do
      typesStr <- (intercalate ", ") <$> mapM stringOfType types
      return $ "(" ++ name ++ " " ++ typesStr ++ " ⇒ " ++ dataTypeStr ++ ")"

instance Show Type where
    showsPrec _ x = shows $ PP.text $ unsafePerformIO $ stringOfType x

instance Eq Type where
  TVar id1 inst1 vname1 == TVar id2 inst2 vname2 = id1 == id2 && instV1 == instV2 && vname1 == vname2 where
    instV1 = readState inst1
    instV2 = readState inst2
  TOper name1 args1 == TOper name2 args2 = name1 == name2 && args1 == args2
  TRecord pairs1 == TRecord pairs2 = pairs1 == pairs2
  TCon name1 types1 dataType1 == TCon name2 types2 dataType2 = name1 == name2 && types1 == types2 && dataType1 == dataType2
  _ == _ = False

instance Ord Type where
    TVar id1 inst1 vname1 <= TVar id2 inst2 vname2 = id1 <= id2 && instV1 <= instV2 && vname1 <= vname2 where
      instV1 = readState inst1
      instV2 = readState inst2
    TOper name1 args1 <= TOper name2 args2 = name1 <= name2 && args1 <= args2
    TRecord pairs1 <= TRecord pairs2 = pairs1 <= pairs2
    TCon name1 types1 dataType1 <= TCon name2 types2 dataType2 = name1 <= name2 && types1 <= types2 && dataType1 <= dataType2
    _ <= _ = False

makeVariable :: Infer Type
makeVariable = do
    i <- nextId
    name <- nextUniqueName
    instRef <- newIORef Nothing
    return $ TVar i instRef name