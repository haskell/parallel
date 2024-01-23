{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module Par (tests) where

import Control.Concurrent (threadDelay)
import Data.Coerce (coerce)
import Data.Foldable (traverse_)
import Data.Functor.Identity
import Test.Tasty
import Test.Tasty.HUnit

import Control.Parallel.Strategies

import Util

data TestStrategy = forall a. TestStrategy
    { name :: TestName
    , value :: () -> a
    , strat :: Strategy a
    , assertEval :: a -> Assertion
    }

testEval :: TestStrategy -> TestTree
testEval (TestStrategy name value strat assertEval) = testCase name $ do
    let a = value ()
    x <- withStrategyIO strat a
    assertEval x

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
test_r0 = TestStrategy { name = "r0", value = const undefined, strat = r0, assertEval = discard }

test_rseq :: TestStrategy
test_rseq = TestStrategy { name = "rseq", value = const (just undefined :: Maybe ()), strat = rseq, assertEval = assertWHNF }

test_rpar :: TestStrategy
test_rpar = TestStrategy { name = "rpar", value = const (undefined :: Maybe ()), strat = rpar, assertEval = \x -> threadDelay 10000 >> assertWHNF x }

test_rdeepseq :: TestStrategy
test_rdeepseq = TestStrategy { name = "rdeepseq", value = const (just (just ())), strat = rdeepseq, assertEval = assertNF }

basicStrategies :: [TestStrategy]
basicStrategies = [test_r0, test_rseq, test_rpar, test_rdeepseq]

testStrategies
    :: (forall a. (() -> a) -> () -> f a)
    -> (forall a. Strategy a -> Strategy (f a))
    -> (forall a. (a -> Assertion) -> f a -> Assertion)
    -> [TestTree]
testStrategies f g h = map (testEval . mapTestStrategy f g h) basicStrategies

testStrategies2
    :: (forall a b. (() -> a) -> (() -> b) -> () -> f a b)
    -> (forall a b. Strategy a -> Strategy b -> Strategy (f a b))
    -> (forall a b. (a -> Assertion) -> (b -> Assertion) -> f a b -> Assertion)
    -> [TestTree]
testStrategies2 f g h = liftA2 (testEval .: map2TestStrategy f g h) basicStrategies basicStrategies
  where
    (.:) = (.) . (.)

tests :: TestTree
tests = testGroup "Control.Parallel.Strategies"
    [ testEval test_r0
    , testEval test_rseq
    , testEval test_rpar
    , testEval test_rdeepseq
    -- FIXME
    --, testGroup "rparWith" $
    --    testStrategies (Identity .) (coerce . rparWith) (\assert -> \x -> threadDelay 10000 >> assert (runIdentity x))
    , testGroup "evalTraversable" $
        testStrategies (\v -> \_ -> [v (), v ()]) evalTraversable traverse_
    , testGroup "evalTuple2" $
        testStrategies2 (\v1 v2 -> \_ -> (v1 (), v2 ())) evalTuple2 (\assert1 assert2 -> \(x, y) -> assert1 x >> assert2 y)
    ]
