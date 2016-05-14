module Main where

import Eval (eval)
import Infer (analyze)
import Parser (parseExpr)
import Value (ValueScope(..))
import TypeScope (TypeScope(..))
import Prologue (assumptions, builtins)
import Control.Lens
import Control.Monad.Trans
import System.Environment
import System.Console.Haskeline
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Exception as E

type Env = (TypeScope, ValueScope)
emptyEnv = (TypeScope Nothing M.empty, ValueScope Nothing M.empty)

loadlib :: IO Env
loadlib = do
  assumps <- assumptions
  std <- readFile "./lib/std.ntha"
  let stdast = parseExpr std
  (stdassumps, _) <- analyze stdast assumps S.empty
  let (stdbuiltins, _) = eval stdast builtins
  return (stdassumps, stdbuiltins)

process :: Env -> String -> IO Env
process (assumps, builtins) expr = E.catch (do
                                            let ast = parseExpr expr
                                            (assumps', t) <- analyze ast assumps S.empty
                                            let (builtins', v) = eval ast builtins
                                            putStrLn $ show v ++ " : " ++ show t
                                            return (assumps', builtins'))
                                           (\(E.ErrorCall e) -> do
                                            putStrLn e
                                            return emptyEnv)

loop :: Env -> InputT IO Env
loop env = do
  minput <- getInputLine "λ> "
  case minput of
    Nothing -> do
      outputStrLn "Goodbye."
      return emptyEnv
    Just input -> (liftIO $ process env input) >>= (\env -> loop env)

prologueMessage :: String
prologueMessage = intercalate "\n"
  ["      _   __   __     __",
   "     / | / /  / /_   / /_   ____ _",
   "    /  |/ /  / __/  / __ \\ / __ `/",
   "   / /|  /  / /_   / / / // /_/ /",
   "  /_/ |_/   \\__/  /_/ /_/ \\__,_/",
   ""
   ]

main :: IO Env
main = do
  env <- loadlib
  args <- getArgs
  case (args ^? element 0) of
    Just arg -> if arg == "repl"
               then do
                putStrLn prologueMessage
                runInputT defaultSettings (loop env)
               else do
                putStrLn "unsupported mode"
                return emptyEnv
    Nothing -> do
      input <- getContents
      process env input
