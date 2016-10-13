-- just add this extentions to make stylish-haskell happy.
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Untyped.Syntax where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Functor.Identity
import qualified Data.Map                      as M
import qualified Text.Parsec.Prim              as P
import           Text.ParserCombinators.Parsec

data Expr = Int Integer
          | Bool Bool
          | Symbol String
          | Fn Function FunctionSignature
          | Special Function FunctionSignature
          | List [Expr]

type FunctionSignature = [String]
type Function = Result

type SymbolTable = M.Map String Expr
data Context = Ctx SymbolTable (Maybe Context)

updateSymbol :: forall m.
                MonadState Context m =>
                String -> Expr -> m ()
updateSymbol s eval_e = modify (\(Ctx sym_table parentCtx) -> Ctx (M.insert s eval_e sym_table) parentCtx)

updateSymbolInParent :: forall m.
                        MonadState Context m =>
                        String -> Expr -> m ()
updateSymbolInParent s eval_e = modify (\(Ctx sym_table parent_ctx) -> (Ctx sym_table (updatedCtx parent_ctx)))
  where updatedCtx (Just (Ctx sym_table ctx)) = Just (Ctx (M.insert s eval_e sym_table) ctx)
        updatedCtx Nothing                    = Nothing

pushContext :: Context -> Context
pushContext ctx = Ctx M.empty (Just ctx)

popContext :: Context -> Context
popContext ctx@(Ctx _ Nothing)      = ctx
popContext (Ctx _ (Just parentCtx)) = parentCtx

type Error = ExceptT String IO
type Result = StateT Context Error Expr

instance Show Expr where
  show (Int x)       = show x
  show (Bool x)      = show x
  show (Symbol x)    = x
  show (Fn _ _)      = "<function>"
  show (Special _ _) = "<special-form>"
  show (List x)      = "(" ++ unwords (map show x) ++ ")"

parseInteger :: forall u. P.ParsecT String u Data.Functor.Identity.Identity Expr
parseInteger = do sign <- option "" (string "-")
                  number <- many1 digit
                  return $ Int (read (sign++number))

parseSymbol :: forall u. P.ParsecT String u Identity Expr
parseSymbol = do f <- firstAllowed
                 r <- many (firstAllowed <|> digit)
                 return $ Symbol (f:r)
  where firstAllowed = oneOf "+-*/" <|> letter

parseExprAux :: P.ParsecT String () Identity Expr
parseExprAux = try parseInteger <|> try parseSymbol <|> try parseList

parseList :: GenParser Char () Expr
parseList = do _ <- char '('
               skipMany space
               x <- parseExprAux `sepEndBy` many1 space
               _ <- char ')'
               return $ List x

parseExpr' :: P.ParsecT String () Identity Expr
parseExpr' = do skipMany space
                x <- parseExprAux
                skipMany space
                eof
                return x

parseExpr :: String -> Result
parseExpr source = case Text.ParserCombinators.Parsec.parse parseExpr' "" source of
                 Right x -> return x
                 Left e  -> throwError $ show e
