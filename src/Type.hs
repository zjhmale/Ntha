{-# OPTIONS_GHC -Wall #-}

module Type where

import State
import Data.IORef
import qualified Data.Map as M

type Id = Int
type TName = String
type TField = String
type Types = [Type]
type TInstance = Maybe Type

data Type = TVar Id (IORef TInstance) TName -- type variable
          | TOper TName Types -- type operator
          | TRecord (M.Map TField Type)
          | TCon TName Types Type
          | TExceptionCon TName Types

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

exceptionT :: Type
exceptionT = TOper "Exception" []

instance Show Type where
  show (TVar _ _ name) = name

instance Eq Type where
  TVar id1 inst1 vname1 == TVar id2 inst2 vname2 = id1 == id2 && instV1 == instV2 && vname1 == vname2 where
    instV1 = readState inst1
    instV2 = readState inst2

instance Ord Type where
    TVar id1 inst1 vname1 <= TVar id2 inst2 vname2 = id1 <= id2 && instV1 <= instV2 && vname1 <= vname2 where
      instV1 = readState inst1
      instV2 = readState inst2

makeVariable :: Infer Type
makeVariable = do
    i <- nextId
    name <- nextUniqueName
    instRef <- newIORef Nothing
    return $ TVar i instRef name