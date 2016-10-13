module UntypedSpec where

import Untyped.Eval
import Untyped.Syntax
import Untyped.Builtins
import Control.Monad.State
import Control.Monad.Except
import Test.Hspec

toplevel' :: [String] -> StateT Context Error String
toplevel' []     = return "empty expressions"
toplevel' [x]    = do expr <- parseExpr x
                      result <- eval expr
                      return $ show result
toplevel' (x:xs) = do expr <- parseExpr x
                      _ <- eval expr
                      toplevel' xs

toplevel :: [String] -> IO (Either String String)
toplevel es = runExceptT $ evalStateT (toplevel' es) initialCtx

spec :: Spec
spec = describe "untyped test" $
        it "should evaluate untyped expressions" $ do
           r1 <- toplevel ["(set fac (fn (n) (if (eq n 1) 1 (* n (fac (- n 1))))))", "(fac 5)"]
           r1 `shouldBe` Right "120"
           r2 <- toplevel ["(set fib (fn (n) (if (eq n 0) 1 (if (eq n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))", "(fib 5)"]
           r2 `shouldBe` Right "8"
