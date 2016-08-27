-- |
-- Assertions provided by libraries *for convenience*
-- It is not hard-coded into Z3.Logic.Pred
--

module Ntha.Z3.Assertion (Assertion(..)) where

import Ntha.Z3.Class
import Ntha.Z3.Encoding()
import Z3.Monad

import qualified Data.Map as M
import qualified Data.Set as S

data Assertion where
    -- | k is mapped to v in (m :: M.Map k v)
    -- XXX: m should be any "term", too strong now
    InMap    :: forall k v. (Z3Sorted k, Z3Encoded k, Z3Sorted v, Z3Reserved v) => k -> v -> M.Map k v -> Assertion
    -- | v is in s
    -- XXX: s should be any "term", too strong now
    InSet    :: forall v. (Z3Encoded v, Z3Sorted v) => v -> S.Set v -> Assertion
    -- | All below are binary relationships
    -- XXX: Should make sure v1 ~ v2, too weak now
    Equal    :: forall v1 v2. (Z3Encoded v1, Z3Encoded v2, Eq v1, Eq v2) => v1 -> v2 -> Assertion
    LessE    :: forall v1 v2. (Z3Encoded v1, Z3Encoded v2, Eq v1, Eq v2) => v1 -> v2 -> Assertion
    GreaterE :: forall v1 v2. (Z3Encoded v1, Z3Encoded v2, Eq v1, Eq v2) => v1 -> v2 -> Assertion
    Less     :: forall v1 v2. (Z3Encoded v1, Z3Encoded v2, Eq v1, Eq v2) => v1 -> v2 -> Assertion
    Greater  :: forall v1 v2. (Z3Encoded v1, Z3Encoded v2, Eq v1, Eq v2) => v1 -> v2 -> Assertion

instance Z3Encoded Assertion where
    encode (InMap k v m) = do
        kTm <- encode k
        vTm <- encode v
        mTm <- encode m
        lhs <- mkSelect mTm kTm
        mkEq lhs vTm
    encode (InSet e s) = do
        eTm <- encode e
        sTm <- encode s
        lhs <- mkSelect sTm eTm
        -- XXX: magic number
        one <- (mkIntSort >>= mkInt 1)
        mkEq one lhs
    encode (Equal t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkEq a1 a2
    encode (LessE t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkLe a1 a2
    encode (GreaterE t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkGe a1 a2
    encode (Less t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkLt a1 a2
    encode (Greater t1 t2) = do
        a1 <- encode t1
        a2 <- encode t2
        mkGt a1 a2
