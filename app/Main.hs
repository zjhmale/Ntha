module Main where

import Eval (eval)
import Infer (analyze)
import Parser (parseExpr)
import Value (ValueScope)
import TypeScope (TypeScope)
import Prologue (assumptions, builtins)
import Control.Lens
import Control.Monad.Trans
import System.Environment
import System.Console.Haskeline
import qualified Data.Set as S
import qualified Control.Exception as E

loadlib :: IO (TypeScope, ValueScope)
loadlib = do
  assumps <- assumptions
  std <- readFile "./lib/std.ntha"
  let stdast = parseExpr std
  (stdassumps, _) <- analyze stdast assumps S.empty
  let (stdbuiltins, _) = eval stdast builtins
  return (stdassumps, stdbuiltins)

process :: String -> IO ()
process expr = E.catch (do
                        (stdassumps, stdbuiltins) <- loadlib
                        let ast = parseExpr expr
                        (_, t) <- analyze ast stdassumps S.empty
                        let (_, v) = eval ast stdbuiltins
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
  loadlib
  args <- getArgs
  case (args ^? element 0) of
    Just arg -> if arg == "repl"
               then runInputT defaultSettings loop
               else putStrLn "unsupported mode"
    Nothing -> do
      input <- getContents
      process input
