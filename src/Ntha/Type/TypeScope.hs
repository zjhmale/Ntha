module Ntha.Type.TypeScope where

import Ntha.Core.Ast
import Ntha.Type.Type
import Prelude hiding (lookup)
import qualified Data.Map as M

type TypeEnv = M.Map EName Type
type ParentScope = TypeScope

data TypeScope = TypeScope (Maybe ParentScope) TypeEnv

createEmptyScope :: TypeScope
createEmptyScope = TypeScope Nothing M.empty

createScopeWithParent :: ParentScope -> TypeScope
createScopeWithParent parent = TypeScope (Just parent) M.empty

createScope :: ParentScope -> TypeEnv -> TypeScope
createScope parent env = TypeScope (Just parent) env

insert :: EName -> Type -> TypeScope -> TypeScope
insert name t (TypeScope parent env) = TypeScope parent (M.insert name t env)

lookup :: EName -> TypeScope -> Maybe Type
lookup name (TypeScope parent env) = case M.lookup name env of
                                      Just t -> Just t
                                      Nothing -> case parent of
                                        Just p -> lookup name p
                                        Nothing -> Nothing

-- create a child type scope of current parent type scope
child :: ParentScope -> TypeScope
child = createScopeWithParent

instance Show TypeScope where
  show (TypeScope parent env) = (show . M.toList) env ++ case parent of
                                              Just p -> " -> " ++ show p
                                              Nothing -> " -| "
