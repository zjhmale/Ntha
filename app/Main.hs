module Main where

import Paths_ntha
import Ntha(
  Expr(..),
  Type(..),
  ValueScope(..),
  Value(..),
  TypeScope(..),
  EPath,
  isImport,
  eval,
  analyze,
  checker,
  parseExpr,
  assumptions,
  builtins)
import Control.Lens
import Control.Monad (foldM)
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

loadFile :: Env -> EPath -> IO Env
loadFile env path = do
  file <- getDataFileName path
  fileContent <- readFile file
  (env', _, _) <- process' env $ parseExpr fileContent
  return env'

loadImport :: Env -> Expr -> IO (Env, Expr)
loadImport env expr = case expr of
  EProgram instructions -> do
    let imports = filter isImport instructions
    let continueAst = EProgram $ filter (not . isImport) instructions
    importEnv <- foldM (\ev (EImport path) -> loadFile ev path) env imports
    return (importEnv, continueAst)
  _ -> return (env, expr)

loadLib :: IO Env
loadLib = do
  assumps <- assumptions
  loadFile (assumps, builtins) "lib/std.ntha"

process' :: Env -> Expr -> IO (Env, Value, Type)
process' env expr = do
   ((importAssumps, importBuiltins), ast) <- loadImport env expr
   (assumps', t) <- analyze ast importAssumps S.empty
   checker ast assumps'
   let (builtins', v) = eval ast importBuiltins
   return ((assumps', builtins'), v, t)

process :: Env -> String -> IO Env
process env@(assumps, prevBuiltins) expr =
  E.catch (do
           (env', v, t) <- process' env $ parseExpr expr
           putStrLn $ show v ++ " : " ++ show t
           return env')
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
                then repl env
                else do
                  file <- readFile arg
                  process env file
    Nothing -> repl env
  where repl ev = do putStrLn prologueMessage
                     runInputT defaultSettings (loop ev)
