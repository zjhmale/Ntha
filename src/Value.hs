{-# OPTIONS_GHC -Wall #-}

module Value where

import Ast
import qualified Data.Map as M

type Tag = String

data Value = VNum Int
           | VStr String
           | VChar Char
           | VBool Bool
           | VList [Value]
           | VTuple [Value]
           | VRecord (M.Map EField Value)
           | VUnit
           | Adt Tag [Value]

nil :: Value
nil = Adt "Nil" []

cons :: Value -> Value -> Value
cons h t = Adt "Cons" [h, t]

instance Show Value where
  show (VNum i) = show i
