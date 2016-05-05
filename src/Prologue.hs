module Prologue where

import Ast
import Type
import State
import TypeScope
import ValueScope
import qualified Data.Map as M

assumptions :: Infer TypeScope
assumptions = do
  return $ TypeScope Nothing $ M.fromList [("+", functionT [intT, intT] intT)]

builtins :: ValueScope
builtins = undefined