module Untyped.Eval where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map             as M
import           Untyped.Syntax

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
        -- like (+ 1 2 3) will fold + on the whole list of arguments
        applyArgsToContext ("...":_) args = updateSymbol "..." (List args)
        -- e.g. (λ x y → x + y) 1 2 will put {x: 1, y: 2} in the context
        applyArgsToContext (earg:expectedArgs) (arg:args) = do updateSymbol earg arg
                                                               applyArgsToContext expectedArgs args
        applyArgsToContext _ _ = return ()
