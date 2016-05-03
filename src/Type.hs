{-# OPTIONS_GHC -Wall #-}

module Type where

import State
import Data.IORef
import qualified Data.Map as M

type TName = String
type TField = String
type Types = [Type]

data Type = TVar TName -- type variable
          | TOper TName (Maybe Types) -- type operator
          | TRecord (M.Map TField Type)
          | TyCon TName Types Type
          | ExceptionCon TName Types

intT :: Type
intT = TOper "Number" Nothing

boolT :: Type
boolT = TOper "Boolean" Nothing

charT :: Type
charT = TOper "Char" Nothing

listT :: Type -> Type -- list type is not polymorphism
listT t = TOper "List" $ Just [t]

productT :: Types -> Type -- tuple type, product type is a name from Algebraic Data type
productT ts = TOper "*" $ Just ts

functionT :: Types -> Type -> Type
functionT args rtn = TOper "->" $ Just $ args ++ [rtn]

strT :: Type
strT = listT charT

unitT :: Type
unitT = TOper "()" Nothing

instance Show Type where
  show (TVar name) = name

makeVariable :: Infer Type
makeVariable = do
    name <- nextUniqueName
    return $ TVar name