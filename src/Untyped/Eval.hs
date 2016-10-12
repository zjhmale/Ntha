module Untyped.Eval where

import Untyped.Syntax
import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Except

eval :: Expr -> Result
eval r@(Int _) = return r
eval r@(Fn _ _) = return r
eval r@(Special _ _) = return r
eval (Symbol s) = do context <- get
                     lookupSymbol context
  where lookupSymbol (Ctx sym_table parentCtx) =
          if s `M.member` sym_table
          then return (sym_table M.! s)
          else case parentCtx of
                 Nothing -> throwError ("Symbol " ++ s ++ " is unbound.")
                 (Just parent) -> lookupSymbol parent

eval r@(List []) = return r
eval (List (x:xs)) = do fn <- eval x
                        apply fn
  where apply (Special f expectedArgs) = apply' expectedArgs xs f
        apply (Fn f expectedArgs) = do args <- mapM eval xs
                                       apply' expectedArgs args f
        apply _ = throwError "First element of a list should be a function or a special form."
        apply' expectedArgs args f = do modify pushContext
                                        applyArgsToContext expectedArgs args
                                        result <- f
                                        modify popContext
                                        return result
        applyArgsToContext ("...":_) args = updateSymbol "..." (List args)
        applyArgsToContext (earg:expectedArgs) (arg:args) = do updateSymbol earg arg
                                                               applyArgsToContext expectedArgs args
        applyArgsToContext _ _ = return ()
