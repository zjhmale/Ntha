module Untyped.Builtins where

import           Control.Monad.State
import           Control.Monad.Except
import qualified Data.Map            as M
import           Untyped.Eval
import           Untyped.Syntax

arithmeticFn :: (Integer -> Integer -> Integer)
             -> StateT Context Error Expr
arithmeticFn f = do (List args) <- getSymbol "..."
                    binaryFn f args

binaryFn :: (Integer -> Integer -> Integer) -> [Expr] -> Result
binaryFn op args = return $ foldl1 (binaryFnAux op) args
  where binaryFnAux op' (Int i) (Int j) = Int (i `op'` j)
        binaryFnAux _ _ _ = Int 0

eqFn :: StateT Context Error Expr
eqFn = do (List args) <- getSymbol "..."
          return $ foldl1 (\(Int a) (Int b) -> Bool $ a == b) args

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
            case eval_cond of
              Bool v -> if v
                        then eval expr1
                        else eval expr2
              _      -> throwError "Cond of if should evaluate to a boolean value"

fnArgs :: [String]
fnArgs = ["args", "..."]

-- | this get newFn part is use the power of lazyness
-- so we can eval the function body until we really need it
-- and at the same time all the arguments are all in the context
-- including the function itself, so recursion is made trivial here.
fn :: StateT Context Error Expr
fn = do [List args, List body] <- getSymbols fnArgs
        let newFn = do evalBody <- mapM eval body
                       case evalBody of
                         [b] -> return b
                         _   -> throwError "invalid function body"
        -- _ <- newFn -- this will definitely cause symbol not found exception
        return $ Fn newFn (map (\(Symbol arg) -> arg) args)

initialCtx :: Context
initialCtx = Ctx (M.fromList [ ("+", Fn (arithmeticFn (+)) ["..."])
                             , ("-", Fn (arithmeticFn (-)) ["..."])
                             , ("*", Fn (arithmeticFn (*)) ["..."])
                             , ("/", Fn (arithmeticFn div) ["..."])
                             , ("eq", Fn eqFn ["..."])
                             , ("set", Special setForm setFormArgs)
                             , ("if", Special ifForm ifFormArgs)
                             , ("fn", Special fn fnArgs)
                             ])
             Nothing

getSymbol :: String -> Result
getSymbol sym = eval $ Symbol sym

getSymbols :: [String] -> StateT Context Error [Expr]
getSymbols = mapM getSymbol
