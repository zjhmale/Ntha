{-# OPTIONS_GHC -Wall #-}

module Type where

import qualified Data.Map as M

type TName = String
type TField = String

data Type = TVar TName
          | TOper TName (Maybe [Type])
          | TRecord (M.Map TField Type)
          | TyCon TName [Type] Type
          | ExceptionCon TName [Type]

intT :: Type
intT = TOper "Number" Nothing

boolT :: Type
boolT = TOper "Boolean" Nothing

charT :: Type
charT = TOper "Char" Nothing

listT :: Type -> Type
listT t = TOper "List" $ Just [t]

productT :: [Type] -> Type
productT ts = TOper "*" $ Just ts

functionT :: [Type] -> Type -> Type
functionT args rtn = TOper "->" $ Just $ args ++ [rtn]

strT :: Type
strT = listT charT

unitT :: Type
unitT = TOper "()" Nothing
