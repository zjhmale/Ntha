module Untyped.Builtins where

import Untyped.Syntax
import Untyped.Eval
import qualified Data.Map as Map
import Control.Monad.State

arithmeticFn :: (Integer -> Integer -> Integer)
             -> StateT Context Error Expr
arithmeticFn f = do (List args) <- getSymbol "..."
                    binaryFn f args

binaryFn :: (Integer->Integer->Integer) -> [Expr] -> Result
binaryFn op args = return $ foldl1 (binaryFnAux op) args
  where binaryFnAux op' (Int i) (Int j) = Int (i `op'` j)
        binaryFnAux _ _ _ = Int 0

eqFn :: StateT Context Error Expr
eqFn = do (List args) <- getSymbol "..."
          return $ foldl1 (\(Int a) (Int b) -> Int(if a == b then 1 else 0)) args

setFormArgs :: [String]
setFormArgs = ["symbol", "value"]

setForm :: StateT Context Error Expr
setForm = do [Symbol s, e] <- getSymbols setFormArgs
             eval_e <- eval e
             updateSymbolInParent s eval_e
             return eval_e

ifFormArgs :: [String]
ifFormArgs = ["condition", "expr1", "expr2"]

ifForm :: StateT Context Error Expr
ifForm = do [condExpr, expr1, expr2] <- getSymbols ifFormArgs
            eval_cond <- eval condExpr
            if 0 `notEqual` eval_cond
              then eval expr1
              else eval expr2
  where notEqual val1 (Int val2) = val1 /= val2
        notEqual _ _ = True

fnArgs :: [String]
fnArgs = ["args", "..."]

fn :: StateT Context Error Expr
fn = do [List args, List body] <- getSymbols fnArgs
        let newFn = do evalBody <- mapM eval body
                       return $ last evalBody
        return $ Fn newFn (map (\(Symbol arg)->arg) args)

initialCtx :: Context
initialCtx = Ctx (Map.fromList [ ("+", Fn (arithmeticFn (+)) ["..."])
                               , ("-", Fn (arithmeticFn (-)) ["..."])
                               , ("*", Fn (arithmeticFn (*)) ["..."])
                               , ("/", Fn (arithmeticFn div) ["..."])
                               , ("eq", Fn eqFn ["..."])
                               , ("set", Special setForm setFormArgs)
                               , ("if", Special ifForm ifFormArgs)
                               , ("fn", Special fn fnArgs )
                               ])
             Nothing

getSymbol :: String -> Result
getSymbol sym = eval $ Symbol sym

getSymbols :: [String] -> StateT Context Error [Expr]
getSymbols = mapM getSymbol
