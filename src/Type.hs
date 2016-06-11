module Type where

import State
import Data.IORef
import Data.List (intercalate)
import Control.Monad (foldM, liftM)
import Data.Maybe (fromMaybe)
import Z3.Class
import Z3.Logic
import Z3.Assertion
import Z3.Monad
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Text.PrettyPrint as PP
import System.IO.Unsafe (unsafePerformIO)

type Id = Int
type TName = String
type TField = String
type Types = [Type]
type TInstance = Maybe Type
type Z3Pred = Pred Term RType Assertion

data Type = TVar Id (IORef TInstance) TName -- type variable
          | TOper TName Types -- type operator
          | TRecord (M.Map TField Type)
          | TCon TName Types Type
          | TSig Type
          | TRefined String Type Term

-- extract normal type from refined type for type inference
extractType :: Type -> Type
extractType t = case t of
                  -- just support arrow type for now
                  TOper "→" args -> TOper "→" (map extractType args)
                  TRefined _ t' _ -> t'
                  _ -> t

extractTerm :: Type -> [Term]
extractTerm t = case t of
                  TOper "→" args -> args >>= extractTerm
                  TRefined _ _ tm -> [tm]
                  _ -> []

getPredNames :: Type -> [String]
getPredNames t = case t of
                   TOper "→" args -> args >>= getPredNames
                   TRefined n _ _ -> [n]
                   _ -> []

intT :: Type
intT = TOper "Number" []

boolT :: Type
boolT = TOper "Boolean" []

charT :: Type
charT = TOper "Char" []

listT :: Type -> Type -- list type is not polymorphism
listT t = TOper "List" [t]

productT :: Types -> Type -- tuple type, product type is a name from Algebraic Data type
productT ts = TOper "*" ts

arrowT :: Type -> Type -> Type -- function type with single param
arrowT fromType toType = TOper "→" $ [fromType, toType]

functionT :: Types -> Type -> Type
functionT paramsT rtnT = foldr (\paramT resT -> arrowT paramT resT) rtnT paramsT

strT :: Type
strT = listT charT

unitT :: Type
unitT = TOper "()" []

prune :: Type -> Infer Type
prune t = case t of
            TVar _ inst _ -> do
              instV <- readIORef inst
              case instV of
                Just inst' -> do
                  newInstance <- prune inst'
                  writeIORef inst $ Just newInstance
                  return newInstance
                Nothing -> return t
            _ -> return t

stringOfType :: M.Map TName TName -> Type -> Infer String
stringOfType subrule (TVar _ inst name) = do
  instV <- readIORef inst
  case instV of
    Just inst' -> stringOfType subrule inst'
    Nothing -> return $ fromMaybe "α" $ M.lookup name subrule
stringOfType subrule (TOper name args) = case name of
                                           "*" -> do
                                             argsStr <- (intercalate " * ") <$> mapM (stringOfType subrule) args
                                             return $ "(" ++ argsStr ++ ")"
                                           "List" -> do
                                             argStr <- stringOfType subrule $ args!!0
                                             return $ "[" ++ argStr ++ "]"
                                           "→" -> do
                                             argT <- prune $ args!!0
                                             rtnT <- prune $ args!!1
                                             argStr <- stringOfType subrule argT
                                             rtnStr <- stringOfType subrule rtnT
                                             let adjust t s = case t of
                                                               TOper "→" _ -> "(" ++ s ++ ")"
                                                               _ -> s
                                             let argStr' = adjust argT argStr
                                             let rtnStr' = adjust rtnT rtnStr
                                             return $ argStr' ++ " → " ++ rtnStr'
                                           _ -> if (length args) == 0
                                               then return name
                                               else do
                                                 argsStr <- unwords <$> mapM (stringOfType subrule) args
                                                 return $ "(" ++ name ++ " " ++ argsStr ++ ")"
stringOfType subrule (TRecord pairs) = do
  pairsStr <- (intercalate ", ") <$> (mapM (\(k, v) -> ((k ++ ": ") ++) <$> stringOfType subrule v) $ M.toList pairs)
  return $ "{" ++ pairsStr ++ "}"
stringOfType subrule (TCon name types dataType) = do
  dataTypeStr <- stringOfType subrule dataType
  case types of
    [] -> return dataTypeStr
    _ -> do
      typesStr <- (intercalate ", ") <$> mapM (stringOfType subrule) types
      return $ "(" ++ name ++ " " ++ typesStr ++ " ⇒ " ++ dataTypeStr ++ ")"
stringOfType subrule (TSig t) = liftM ("typesig: " ++) $ stringOfType subrule t
stringOfType subrule (TRefined _ t _) = liftM ("refined: " ++) $ stringOfType subrule t

getFreeVars :: Type -> Infer (S.Set TName)
getFreeVars (TVar _ inst name) = do
  instV <- readIORef inst
  case instV of
    Just inst' -> getFreeVars inst'
    Nothing -> return $ S.singleton name
getFreeVars (TOper _ args) = foldM (\acc arg -> do
                                     freeVars <- getFreeVars arg
                                     return $ S.union freeVars acc)
                                   S.empty args
getFreeVars (TRecord pairs) = foldM (\acc (_, v) -> do
                                      freeVars <- getFreeVars v
                                      return $ S.union freeVars acc)
                                    S.empty $ M.toList pairs
getFreeVars (TCon _ types dataType) = foldM (\acc t -> do
                                              freeVars <- getFreeVars t
                                              return $ S.union freeVars acc)
                                            S.empty $ types ++ [dataType]
getFreeVars (TSig t) = getFreeVars t
getFreeVars (TRefined _ t _) = getFreeVars t

normalize :: Type -> Infer String
normalize t = do
  freeVars <- getFreeVars t
  let subrule = M.map (\c -> [c]) $ M.fromList $ zip (S.toList freeVars) ['α'..'ω']
  stringOfType subrule t

instance Show Type where
    showsPrec _ x = shows $ PP.text $ unsafePerformIO $ normalize x

instance Eq Type where
  TVar id1 inst1 vname1 == TVar id2 inst2 vname2 = id1 == id2 && instV1 == instV2 && vname1 == vname2 where
    instV1 = readState inst1
    instV2 = readState inst2
  TOper name1 args1 == TOper name2 args2 = name1 == name2 && args1 == args2
  TRecord pairs1 == TRecord pairs2 = pairs1 == pairs2
  TCon name1 types1 dataType1 == TCon name2 types2 dataType2 = name1 == name2 && types1 == types2 && dataType1 == dataType2
  TSig t1 == TSig t2 = t1 == t2
  TRefined x1 t1 tm1 == TRefined x2 t2 tm2 = x1 == x2 && t1 == t2 && tm1 == tm2
  _ == _ = False

instance Ord Type where
    TVar id1 inst1 vname1 <= TVar id2 inst2 vname2 = id1 <= id2 && instV1 <= instV2 && vname1 <= vname2 where
      instV1 = readState inst1
      instV2 = readState inst2
    TOper name1 args1 <= TOper name2 args2 = name1 <= name2 && args1 <= args2
    TRecord pairs1 <= TRecord pairs2 = pairs1 <= pairs2
    TCon name1 types1 dataType1 <= TCon name2 types2 dataType2 = name1 <= name2 && types1 <= types2 && dataType1 <= dataType2
    TSig t1 <= TSig t2 = t1 <= t2
    TRefined x1 t1 tm1 <= TRefined x2 t2 tm2 = x1 <= x2 && t1 <= t2 && tm1 <= tm2
    _ <= _ = False

makeVariable :: Infer Type
makeVariable = do
    i <- nextId
    name <- nextUniqueName
    instRef <- newIORef Nothing
    return $ TVar i instRef name

-- for refined type

data Term = TmVar   String
          | TmNum   Int
          | TmLT    Term Term
          | TmGT    Term Term
          | TmLE    Term Term
          | TmGE    Term Term
          | TmSub   Term Term
          | TmAdd   Term Term
          | TmMul   Term Term
          | TmDiv   Term Term
          | TmEqual Term Term
          | TmAnd   Term Term
          | TmOr    Term Term
          | TmNot   Term
          | TmIf    Term Term Term

deriving instance Eq Term
deriving instance Ord Term

-- currently just support integer
data RType = RTInt

deriving instance Eq RType
deriving instance Ord RType

instance Z3Encoded Term where
    encode (TmVar x) = do
        ctx <- getQualifierCtx
        case M.lookup x ctx of
            Just (idx, _) -> return idx
            Nothing -> smtError $ "Can't find variable " ++ x
    encode (TmNum n) = mkIntSort >>= mkInt n
    encode (TmLT t1 t2) = encode (Less t1 t2)
    encode (TmGT t1 t2) = encode (Greater t1 t2)
    encode (TmLE t1 t2) = encode (LessE t1 t2)
    encode (TmGE t1 t2) = encode (GreaterE t1 t2)
    encode (TmAdd t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkAdd [a1, a2]
    encode (TmSub t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkSub [a1, a2]
    encode (TmMul t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkMul [a1, a2]
    encode (TmDiv t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkDiv a1 a2
    encode (TmEqual t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkEq a1 a2
    encode (TmAnd t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkAnd [a1, a2]
    encode (TmOr t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkOr [a1, a2]
    encode (TmNot t) = encode t >>= mkNot
    encode (TmIf p c a) = do
        a1 <- encode p
        a2 <- encode c
        a3 <- encode a
        mkIte a1 a2 a3

instance Z3Sorted Term where
    sort (TmVar x) = do
        ctx <- getQualifierCtx
        case M.lookup x ctx of
            Just (_, s) -> return s
            Nothing -> smtError $ "Can't find variable " ++ x
    sort (TmNum _) = mkIntSort
    sort (TmLT _ _) = mkBoolSort
    sort (TmGT _ _) = mkBoolSort
    sort (TmLE _ _) = mkBoolSort
    sort (TmGE _ _) = mkBoolSort
    sort (TmAdd _ _) = mkIntSort
    sort (TmSub _ _) = mkIntSort
    sort (TmMul _ _) = mkIntSort
    sort (TmDiv _ _) = mkIntSort
    sort (TmEqual _ _) = mkBoolSort
    sort (TmAnd _ _) = mkBoolSort
    sort (TmOr _ _) = mkBoolSort
    sort (TmNot _) = mkBoolSort
    sort (TmIf _ c _) = sort c

instance Z3Sorted RType where
    sort RTInt  = mkIntSort
