{-# OPTIONS_GHC -Wall #-}

module ValueScope where

import Ast
import Value
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
child :: ParentScope -> ValueScope
child = createScopeWithParent

instance Show ValueScope where
  show (ValueScope parent env) = (show . M.toList) env ++ case parent of
                                              Just p -> " -> " ++ show p
                                              Nothing -> " -| "
