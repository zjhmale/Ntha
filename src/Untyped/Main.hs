module Untyped.Main where

import Untyped.Eval
import Untyped.Syntax
import Untyped.Builtins
import System.IO
import Control.Monad
import Control.Monad.State
import Control.Monad.Except

main :: IO ()
main = do _ <- runExceptT (evalStateT repl initialCtx)
          return ()

repl :: StateT Context Error ()
repl = do liftIO $ putStr "λ> "
          liftIO $ hFlush stdout
          x <- liftIO getLine
          unless (x == "(quit)") $
            do expr <- parseExpr x
               evaledExpr <- eval expr
               liftIO $ print evaledExpr
               repl
            `catchError` (\e -> do liftIO $ putStrLn e
                                   repl)
