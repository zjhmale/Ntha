{-# OPTIONS_GHC -Wall #-}

module Value where

import Ast
import Data.List (intercalate)
import Prelude hiding (lookup)
import qualified Data.Map as M

type ValueEnv = M.Map EName Value
type ParentScope = ValueScope

data ValueScope = ValueScope (Maybe ParentScope) ValueEnv

createEmptyScope :: ValueScope
createEmptyScope = ValueScope Nothing M.empty

createScopeWithParent :: ParentScope -> ValueScope
createScopeWithParent parent = ValueScope (Just parent) M.empty

createScope :: ParentScope -> ValueEnv -> ValueScope
createScope parent env = ValueScope (Just parent) env

insert :: EName -> Value -> ValueScope -> ValueScope
insert name t (ValueScope parent env) = ValueScope parent (M.insert name t env)

lookup :: EName -> ValueScope -> Maybe Value
lookup name (ValueScope parent env) = case M.lookup name env of
                                      Just t -> Just t
                                      Nothing -> case parent of
                                        Just p -> lookup name p
                                        Nothing -> Nothing

-- create a child type scope of current parent type scope
-- just to mock immutable scope, will remove later
child :: ParentScope -> ValueScope
child = createScopeWithParent

instance Show ValueScope where
  show (ValueScope parent env) = (show . M.toList) env ++ case parent of
                                              Just p -> " -> " ++ show p
                                              Nothing -> " -| "

type Tag = String
type FreeVal = Value

data Value = VNum Int
           | VChar Char
           | VBool Bool
           | VTuple [Value]
           | VRecord (M.Map EField Value)
           | VUnit
           | Adt Tag [Value]
           | Fn (Value -> ValueScope -> Value) -- or closure
           | FnApArgs (M.Map String Value)
           | DestrFnApArgs [PatVal] FreeVal
           | TConArgs [Value] Tag

data PatVal = PatVal Pattern Value
              deriving (Eq, Show)

nil :: Value
nil = Adt "Nil" []

cons :: Value -> Value -> Value
cons h t = Adt "Cons" [h, t]

-- binary operator
binFn :: (Value -> Value -> Value) -> Value
binFn f = Fn (\arg1 _ -> Fn (\arg2 _ -> f arg1 arg2))

stringOfAdt :: Tag -> [Value] -> String
stringOfAdt tag values = tag ++ case values of
                                  [] -> ""
                                  _ -> " " ++ intercalate " | " (map show values)

stringOfPairs :: M.Map String Value -> String
stringOfPairs pairs = "{" ++ intercalate "," (M.elems $ M.mapWithKey (\f v -> f ++ ": " ++ show v) pairs) ++ "}"

instance Show Value where
  show (VNum i) = show i
  show (VChar c) = [c]
  show (VBool b) = show b
  show (VTuple values) = "(" ++ intercalate "," (map show values) ++ ")"
  show (VRecord pairs) = stringOfPairs pairs
  show VUnit = "⊥"
  show (Adt tag values) = stringOfAdt tag values
  show (Fn _) = "<fun>"
  show (FnApArgs pairs) = "FnApArgs(" ++ stringOfPairs pairs ++ ")"
  show (DestrFnApArgs pats val) = "DestrFnApArgs(" ++ intercalate ", " (map show pats) ++ " * " ++ show val ++ ")"
  show (TConArgs values tag) = "TConArgs(" ++ stringOfAdt tag values ++ ")"

instance Eq Value where
  VNum int1 == VNum int2 = int1 == int2
  VChar char1 == VChar char2 = char1 == char2
  VBool bool1 == VBool bool2 = bool1 == bool2
  VTuple values1 == VTuple values2 = values1 == values2
  VRecord pairs1 == VRecord pairs2 = pairs1 == pairs2
  VUnit == VUnit = True
  Adt tag1 values1 == Adt tag2 values2 = tag1 == tag2 && values1 == values2
  FnApArgs pairs1 == FnApArgs pairs2 = pairs1 == pairs2
  DestrFnApArgs vals1 val1 == DestrFnApArgs vals2 val2 = vals1 == vals2 && val1 == val2
  TConArgs vals1 tag1 == TConArgs vals2 tag2 = vals1 == vals2 && tag1 == tag2
  _ == _ = False