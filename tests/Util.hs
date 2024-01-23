{-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module Util
    ( isNF
    , isWHNF
    , discard
    , assertNF
    , assertWHNF
    , just
    ) where

import Data.Maybe (isNothing)
import Data.Typeable (Typeable)
import NoThunks.Class

import Test.Tasty.HUnit

isNF :: (NoThunks a) => a -> IO Bool
isNF x = isNothing <$> noThunks [] x

isWHNF :: (Typeable a) => a -> IO Bool
isWHNF x = isNothing <$> noThunks [] (OnlyCheckWhnf x)

discard :: a -> Assertion
discard _ = pure ()

assertNF :: (NoThunks a) => a -> Assertion
assertNF x = isNF x @? "value is not in NF"

assertWHNF :: (Typeable a) => a -> Assertion
assertWHNF x = isWHNF x @? "value is not in WHNF"

-- used to create a thunk
just :: a -> Maybe a
just = Just
{-# NOINLINE just #-}
