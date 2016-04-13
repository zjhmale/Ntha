module Lib
    ( someFunc
    ) where

import qualified Data.Map as M
import Ast
import Type

someMap :: M.Map String String
someMap = M.fromList [("stack exec", "hello haskell")]

someFunc :: IO ()
someFunc = print $ M.lookup "stack exec" someMap
