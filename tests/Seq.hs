{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module Seq (tests) where

import qualified Data.Array as A
import Data.Foldable (traverse_)
import qualified Data.Map as Map

import Test.Tasty
import Test.Tasty.HUnit

import Control.Seq

import Util

data TestStrategy = forall a. TestStrategy
    { name :: TestName
    , value :: () -> a
    , strat :: Strategy a
    , assertSeq :: a -> Assertion
    }

testSeq :: TestStrategy -> TestTree
testSeq (TestStrategy name value strat assertSeq) = testCase name $ do
    let a = value ()
    strat a @?= ()
    assertSeq a

mapTestStrategy
    :: (forall a. (() -> a) -> () -> f a)
    -> (forall a. Strategy a -> Strategy (f a))
    -> (forall a. (a -> Assertion) -> f a -> Assertion)
    -> TestStrategy
    -> TestStrategy
mapTestStrategy f g h (TestStrategy name value strat assertSeq) = TestStrategy name (f value) (g strat) (h assertSeq)

map2TestStrategy
    :: (forall a b. (() -> a) -> (() -> b) -> () -> f a b)
    -> (forall a b. Strategy a -> Strategy b -> Strategy (f a b))
    -> (forall a b. (a -> Assertion) -> (b -> Assertion) -> f a b -> Assertion)
    -> TestStrategy
    -> TestStrategy
    -> TestStrategy
map2TestStrategy f g h (TestStrategy name1 value1 strat1 assertSeq1) (TestStrategy name2 value2 strat2 assertSeq2) =
    TestStrategy (name1 ++ " " ++ name2) (f value1 value2) (g strat1 strat2) (h assertSeq1 assertSeq2)

test_r0 :: TestStrategy
test_r0 = TestStrategy { name = "r0", value = const undefined, strat = r0, assertSeq = discard }

test_rseq :: TestStrategy
test_rseq = TestStrategy { name = "rseq", value = const (just undefined :: Maybe ()), strat = rseq, assertSeq = assertWHNF }

test_rdeepseq :: TestStrategy
test_rdeepseq = TestStrategy { name = "rdeepseq", value = const (just (just ())), strat = rdeepseq, assertSeq = assertNF }

basicStrategies :: [TestStrategy]
basicStrategies = [test_r0, test_rseq, test_rdeepseq]

testStrategies
    :: (forall a. (() -> a) -> () -> f a)
    -> (forall a. Strategy a -> Strategy (f a))
    -> (forall a. (a -> Assertion) -> f a -> Assertion)
    -> [TestTree]
testStrategies f g h = map (testSeq . mapTestStrategy f g h) basicStrategies

testStrategies2
    :: (forall a b. (() -> a) -> (() -> b) -> () -> f a b)
    -> (forall a b. Strategy a -> Strategy b -> Strategy (f a b))
    -> (forall a b. (a -> Assertion) -> (b -> Assertion) -> f a b -> Assertion)
    -> [TestTree]
testStrategies2 f g h = liftA2 (testSeq .: map2TestStrategy f g h) basicStrategies basicStrategies
  where
    (.:) = (.) . (.)

tests :: TestTree
tests = testGroup "Control.Seq" $
    [ testSeq test_r0
    , testSeq test_rseq
    , testSeq test_rdeepseq
    , testGroup "seqList" $
        testStrategies (\v -> \_ -> [v (), v ()]) seqList traverse_
    , testGroup "seqFoldable" $
        testStrategies (\v -> \_ -> Just (v ())) seqFoldable traverse_
    , testGroup "seqMap" $
        -- keys are ignored, since a Map is strict in its keys anyway
        testStrategies (\v -> \_ -> Map.fromList [(1 :: Int, v ()), (2, v ())]) (seqMap r0) (\assertSeq -> traverse_ (\(_k, v) -> assertSeq v) . Map.toList)
    , testGroup "seqArray" $
        testStrategies (\v -> \_ -> A.listArray (0 :: Int, 1) [v (), v ()]) seqArray traverse_
    , testGroup "seqTuple2" $
        testStrategies2 (\v1 v2 -> \_ -> (v1 (), v2 ())) seqTuple2 (\assert1 assert2 -> \(x, y) -> assert1 x >> assert2 y)
    ]
