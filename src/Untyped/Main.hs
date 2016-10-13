module Untyped.Main where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State
import           System.IO
import           Untyped.Builtins
import           Untyped.Eval
import           Untyped.Syntax

main :: IO ()
main = do _ <- runExceptT $ evalStateT repl initialCtx
          return ()

repl :: StateT Context Error ()
repl = do liftIO $ putStr "λ> "
          liftIO $ hFlush stdout
          x <- liftIO getLine
          unless (x == "(quit)") $
            do expr <- parseExpr x
               result <- eval expr
               liftIO $ print result
               repl
            `catchError` (\e -> do liftIO $ putStrLn e
                                   repl)
