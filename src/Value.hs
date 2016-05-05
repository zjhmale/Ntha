module Value where

import Ast
import qualified Data.Map as M

data Value = VNum Int
           | VStr String
           | VChar Char
           | VBool Bool
           | VList [Value]
           | VTuple [Value]
           | VRecord (M.Map EField Value)
           | VUnit

instance Show Value where
  show (VNum i) = show i
