module Main where

import Eval (eval)
import Infer (analyze)
import Parser (parseExpr)
import Prologue (assumptions, builtins)
import Control.Lens
import Control.Monad.Trans
import System.Environment
import System.Console.Haskeline
import qualified Data.Set as S
import qualified Control.Exception as E

process :: String -> IO ()
process expr = E.catch (do
                        assumps <- assumptions
                        let ast = parseExpr expr
                        (_, t) <- analyze ast assumps S.empty
                        let (_, v) = eval ast builtins
                        putStrLn $ show v ++ " : " ++ show t)
                       (\(E.ErrorCall e) -> putStrLn e)

loop :: InputT IO ()
loop = do
  minput <- getInputLine "λ> "
  case minput of
    Nothing -> outputStrLn "Goodbye."
    Just input -> (liftIO $ process input) >> loop

main :: IO ()
main = do
  args <- getArgs
  case (args ^? element 0) of
    Just arg -> if arg == "repl"
               then runInputT defaultSettings loop
               else putStrLn "unsupported mode"
    Nothing -> do
      input <- getContents
      process input
