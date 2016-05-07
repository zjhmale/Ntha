{-# OPTIONS_GHC -Wall #-}

module Value where

import Ast
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

data PatVal = PatVal Pattern Value

nil :: Value
nil = Adt "Nil" []

cons :: Value -> Value -> Value
cons h t = Adt "Cons" [h, t]

-- binary operator
binFn :: (Value -> Value -> Value) -> Value
binFn f = Fn (\arg1 _ -> Fn (\arg2 _ -> f arg1 arg2))

instance Show Value where
  show (VNum i) = show i
