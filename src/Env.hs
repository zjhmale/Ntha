module BP.Env where

import Ast
import Type
import State
import System.IO.Unsafe(unsafePerformIO)
import qualified Data.Map as M

type Env = M.Map EName Type
