module Main where

import Ast (EPath)
import Eval (eval)
import Infer (analyze)
import Refined (checker)
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

emptyEnv :: (TypeScope, ValueScope)
emptyEnv = (TypeScope Nothing M.empty, ValueScope Nothing M.empty)

loadFile :: EPath -> IO Env
loadFile path = do
  assumps <- assumptions
  std <- readFile path
  let stdast = parseExpr std
  (stdassumps, _) <- analyze stdast assumps S.empty
  checker stdast stdassumps
  let (stdbuiltins, _) = eval stdast builtins
  return (stdassumps, stdbuiltins)

loadLib :: IO Env
loadLib = loadFile "./lib/std.ntha"

process :: Env -> String -> IO Env
process (assumps, prevBuiltins) expr = E.catch (do
                                            let ast = parseExpr expr
                                            (assumps', t) <- analyze ast assumps S.empty
                                            checker ast assumps'
                                            let (builtins', v) = eval ast prevBuiltins
                                            putStrLn $ show v ++ " : " ++ show t
                                            return (assumps', builtins'))
                                           (\(E.ErrorCall e) -> do
                                            putStrLn e
                                            return (assumps, prevBuiltins))

loop :: Env -> InputT IO Env
loop env = do
  minput <- getInputLine "λ> "
  case minput of
    Nothing -> do
      outputStrLn "Goodbye."
      return emptyEnv
    Just input -> (liftIO $ process env input) >>= (\env' -> loop env')

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
  env <- loadLib
  args <- getArgs
  case (args ^? element 0) of
    Just arg -> if arg == "repl"
               then do
                putStrLn prologueMessage
                runInputT defaultSettings (loop env)
               else do
                file <- readFile arg
                process env file
    Nothing -> do
      input <- getContents
      process env input
