{-# LANGUAGE BangPatterns, CPP, MagicHash, UnboxedTuples #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Parallel.Strategies.Poly
-- Copyright   :  (c) The University of Glasgow 2001-2010
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Parallel evaluation strategies, or strategies for short, provide
-- ways to express parallel computations. This module provides strategies
-- that can transform values of one type into values of another. Strategies
-- have the following key features:
--
--  * Strategies express /deterministic parallelism/:
--    the result of the program is unaffected by evaluating in parallel.
--    The parallel tasks evaluated by a strategy may have no side effects.
--    For non-deterministic parallel programming, see "Control.Concurrent".
--
--  * Strategies are /compositional/: larger strategies can be built
--    by gluing together smaller ones.
--
--  * 'Monad' and 'Applicative' instances are provided, for quickly building
--    strategies that involve traversing structures in a regular way.
--
--  * While the strategies in "Control.Parallel.Strategies"
--    are designed to separate the description of the parallelism from the
--    logic of your program, the strategies in this module
--    express a notion of strategy as a way to use sequential and parallel
--    computation to calculate one value from another.
--
-- Note that the 'evalListN', 'parListN', 'evalListNth', 'parListNth',
-- 'evalListSplitAt', and 'parListSplitAt'
-- strategies have been omitted from this module, as they don't seem to be
-- a great match for its general philosophy. The deprecated functions in
-- "Control.Parallel.Strategies" have also been omitted.

-----------------------------------------------------------------------------

module Control.Parallel.Strategies.Poly (
         -- * The strategy type
         Strategy

         -- * Application of strategies
       , using             -- :: a -> Strategy a b -> b
       , withStrategy      -- :: Strategy a b -> a -> b
       , usingIO           -- :: a -> Strategy a b -> IO b
       , withStrategyIO    -- :: Strategy a b -> a -> IO b

         -- * Composition of strategies
       , dot               -- :: Strategy b c -> Strategy a b -> Strategy a c

         -- * Basic strategies
       , r0                -- :: Strategy a a
       , rseq
       , rdeepseq          -- :: NFData a => Strategy a a
       , rpar              -- :: Strategy a a
       , rparWith          -- :: Strategy a b -> Strategy a b

         -- * Injection of sequential strategies
       , evalSeq           -- :: Seq.Strategy a -> Strategy a a
       , SeqStrategy

         -- * Strategies for traversable data types
       , evalTraversable   -- :: Traversable t => Strategy a b -> Strategy (t a) (t b)
       , parTraversable

         -- * Strategies for lists
       , evalList          -- :: Strategy a b -> Strategy [a] [b]
       , parList
       , parListChunk
       , parMap

         -- ** Strategies for lazy lists
       , evalBuffer        -- :: Int -> Strategy a b -> Strategy [a] [b]
       , parBuffer

         -- * Strategies for tuples

         -- | Calculate a tuple component-wise from another tuple using
         -- a strategy for each component.

       , evalTuple2        -- :: Strategy a b -> ... -> Strategy (a,...) (b,...)
       , evalTuple3
       , evalTuple4
       , evalTuple5
       , evalTuple6
       , evalTuple7
       , evalTuple8
       , evalTuple9


       -- | Calculate a tuple component-wise, in parallel, from another tuple
       -- using a strategy for each component.

       , parTuple2         -- :: Strategy a b -> ... -> Strategy (a,...) (b,...)
       , parTuple3
       , parTuple4
       , parTuple5
       , parTuple6
       , parTuple7
       , parTuple8
       , parTuple9

         -- * Strategic function application
       , ($|)              -- :: (b -> c) -> Strategy a b -> a -> c
       , ($||)
       , (.|)              -- :: (c -> d) -> Strategy b c -> (a -> b) -> a -> d
       , (.||)
       , (-|)              -- :: (a -> b) -> Strategy b c -> (c -> d) -> a -> d
       , (-||)

         -- * For Strategy programmers
       , Eval              -- instances: Monad, Functor, Applicative
       , runEval           -- :: Eval a -> a
       , runEvalIO         -- :: Eval a -> IO a
       ,
  ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Traversable
import Control.Applicative
#endif
import Control.Parallel
import Control.DeepSeq (NFData(rnf))

#if MIN_VERSION_base(4,4,0)
import System.IO.Unsafe (unsafeDupablePerformIO)
import Control.Exception (evaluate)
#else
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad
#endif

import qualified Control.Seq

import GHC.Exts
import GHC.IO (IO (..))

infixr 9 `dot`     -- same as (.)
infixl 0 `using`   -- lowest precedence and associate to the left
infixl 0 `usingIO` -- lowest precedence and associate to the left

-- -----------------------------------------------------------------------------
-- Eval monad (isomorphic to Lift monad from MonadLib 3.6.1)

-- | 'Eval' is a Monad that makes it easier to define parallel
-- strategies.  It is a strict identity monad: that is, in
--
--  > m >>= f
--
-- @m@ is evaluated before the result is passed to @f@.
--
--  > instance Monad Eval where
--  >   return  = Done
--  >   m >>= k = case m of
--  >               Done x -> k x
--
-- If you wanted to construct a 'Strategy' for a pair that sparked the
-- first component in parallel and then evaluated the second
-- component, you could write
--
-- > myStrat :: Strategy (a,b)
-- > myStrat (a,b) = do { a' <- rpar a; b' <- rseq b; return (a',b') }
--
-- Alternatively, you could write this more compactly using the
-- Applicative style as
--
-- > myStrat (a,b) = (,) <$> rpar a <*> rseq b

-- More examples, using the Applicative instance:
--
-- > parList :: Strategy a -> Strategy [a]
-- > parList strat = traverse (rpar `dot` strat))
--
-- > evalPair :: Strategy a -> Strategy b -> Strategy (a,b)
-- > evalPair f g (a,b) = pure (,) <$> f a <*> g b
--

#if __GLASGOW_HASKELL__ >= 702

newtype Eval a = Eval {unEval_ :: IO a}
  deriving (Functor, Applicative, Monad)
  -- GHC 7.2.1 added the seq# and spark# primitives, that we use in
  -- the Eval monad implementation in order to get the correct
  -- strictness behaviour.

-- | Pull the result out of the monad.
runEval :: Eval a -> a
#  if MIN_VERSION_base(4,4,0)
runEval = unsafeDupablePerformIO . unEval_
#  else
runEval = unsafePerformIO . unEval_
#  endif

-- | Run the evaluation in the 'IO' monad. This allows sequencing of
-- evaluations relative to 'IO' actions.
runEvalIO :: Eval a -> IO a
runEvalIO = unEval_

#else

data Eval a = Done a

-- | Pull the result out of the monad.
runEval :: Eval a -> a
runEval (Done x) = x

-- | Run the evaluation in the 'IO' monad. This allows sequencing of
-- evaluations relative to 'IO' actions.
runEvalIO :: Eval a -> IO a
runEvalIO (Done x) = return x

instance Functor Eval where
  fmap = liftM

instance Applicative Eval where
  pure = Done
  (<*>) = ap

instance Monad Eval where
  return = pure
  Done x >>= k = lazy (k x)   -- Note: pattern 'Done x' makes '>>=' strict

{-# RULES "lazy Done" forall x . lazy (Done x) = Done x #-}

-- The Eval monad satisfies the monad laws.
--
-- (1) Left identity:
--     return x >>= f ==> Done x >>= f ==> f x
--
-- (2) Right identity:
--     (i)  m >>= return =*> Done u >>= return
--                       ==> return u
--                       ==> Done u <*= m
--     (ii) m >>= return =*> undefined >>= return
--                       ==> undefined <*= m
--
-- (3) Associativity:
--     (i)  (m >>= f) >>= g =*> (Done u >>= f) >>= g
--                          ==> f u >>= g <== (\x -> f x >>= g) u
--                                        <== Done u >>= (\x -> f x >>= g)
--                                        <*= m >>= (\x -> f x >>= g)
--     (ii) (m >>= f) >>= g =*> (undefined >>= f) >>= g
--                          ==> undefined >>= g
--                          ==> undefined <== undefined >>= (\x -> f x >>= g)
--                                        <*= m >>= (\x -> f x >>= g)

#endif


-- -----------------------------------------------------------------------------
-- Strategies

-- | A 'Strategy' is a function that embodies a parallel evaluation strategy.
-- The function uses parallel and/or sequential computations to calculate
-- a value from its argument.
--
-- Parallel computations may be discarded by the runtime system if the
-- program no longer requires their result. As a consequence, a strategy
-- should not be used to evaluate its argument unless its result will be
-- used as well. In most cases, the argument should simply be discarded,
-- an idiom expressed by the 'using' function.
--
-- === Incorrect use
--
-- @
-- do
--   _ <- rpar x
--   ...
--   ... x ...
-- @
--
-- In the above, the spark to evaluate @x@ in parallel will be discarded
-- because its result is not used.
--
-- === Correct uses
--
-- @
-- do
--   x' <- rpar x
--   ...
--   ... x' ...
-- @
--
-- In this example, @x@ will be evaluated in parallel, and the result of
-- doing so will be used as @x'@.
--
-- @
-- do
--   xs' <- evalList (rpar . (f $!)) xs
--   ...
--   ... xs ... xs' ...
-- @
--
-- In this example, the original list is reused after strategy application,
-- but the list that was evaluated in parallel is used as well, so the
-- parallel computations will not be discarded.

type Strategy a b = a -> Eval b

-- | Evaluate a value using the given 'Strategy'.
--
-- > x `using` s = runEval (s x)
--
using :: a -> Strategy a b -> b
x `using` strat = runEval (strat x)

-- | Evaluate a value using the given 'Strategy'.  This is simply
-- 'using' with the arguments reversed.
--
withStrategy :: Strategy a b -> a -> b
withStrategy = flip using

-- | Evaluate a value using the given 'Strategy' inside the 'IO' monad.  See
-- also 'runEvalIO'.
--
-- > x `usingIO` s = runEvalIO (s x)
--
usingIO :: a -> Strategy a b -> IO b
x `usingIO` strat = runEvalIO (strat x)

-- | Evaluate a value using the given 'Strategy' inside the 'IO' monad.  This
-- is simply 'usingIO' with the arguments reversed.
--
withStrategyIO :: Strategy a b -> a -> IO b
withStrategyIO = flip usingIO

-- | Compose two strategies sequentially.
-- This is the analogue to function composition on strategies.
--
-- > (strat1 `dot` strat2) `dot` strat3 == strat1 `dot` (strat2 `dot` strat3)
-- > strat `dot` r0 == strat
-- > strat `dot` rpar == strat
-- > strat `dot` rseq == strat
--
-- > strat2 `dot` strat1 == strat2 . withStrategy strat1
--
dot :: Strategy b c -> Strategy a b -> Strategy a c
strat2 `dot` strat1 = strat2 . runEval . strat1

-- Proof of strat2 `dot` strat1 == strat2 . withStrategy strat1
--
--    strat2 . withStrategy strat1
-- == \x -> strat2 (withStrategy strat1 x)
-- == \x -> strat2 (x `using` strat1)
-- == \x -> strat2 (runEval (strat1 x))
-- == \x -> (strat2 . runEval . strat1) x
-- == strat2 `dot` strat1

-- One might be tempted to think that 'dot' is equivalent to '(<=<)',
-- the right-to-left Kleisli composition in the Eval monad, because
-- '(<=<)' can take the type @Strategy a -> Strategy a -> Strategy a@
-- and intuitively does what 'dot' does: First apply the strategy to the
-- right then the one to the left. However, there is a subtle difference
-- in strictness, witnessed by the following example:
--
-- > (r0 `dot` rseq) undefined == Done undefined
-- > (r0 <=< rseq) undefined == undefined
--

-- | Inject a sequential strategy (ie. coerce a sequential strategy
-- to a general strategy).
--
-- Thanks to 'evalSeq', the type @Control.Seq.Strategy a@ is a subtype
-- of @'Strategy' a@.
evalSeq :: SeqStrategy a -> Strategy a a
evalSeq strat x = rseq (strat x) >> return x

-- | A name for @Control.Seq.Strategy@, for documentation only.
type SeqStrategy a = Control.Seq.Strategy a

-- --------------------------------------------------------------------------
-- Basic strategies (some imported from SeqStrategies)

-- | 'r0' performs *no* evaluation.
--
-- > r0 == evalSeq Control.Seq.r0
--
r0 :: Strategy a a
r0 x = return x

-- Proof of r0 == evalSeq Control.Seq.r0
--
--    evalSeq Control.Seq.r0
-- == \x -> Control.Seq.r0 x `pseq` return x
-- == \x -> Control.Seq.Done `pseq` return x
-- == \x -> return x
-- == r0

-- | 'rseq' evaluates its argument to weak head normal form.
--
-- > rseq == evalSeq Control.Seq.rseq
--
rseq :: Strategy a a
#if __GLASGOW_HASKELL__ >= 702
rseq x = Eval (evaluate x)
#else
rseq x = x `seq` return x
#endif
-- Staged NOINLINE so we can match on rseq in RULES
{-# NOINLINE [1] rseq #-}


-- Proof of rseq == evalSeq Control.Seq.rseq
--
--    evalSeq Control.Seq.rseq
-- == \x -> Control.Seq.rseq x `pseq` return x
-- == \x -> (x `seq` Control.Seq.Done) `pseq` return x
-- == \x -> x `pseq` return x
-- == rseq

-- | 'rdeepseq' fully evaluates its argument.
--
-- > rdeepseq == evalSeq Control.Seq.rdeepseq
--
rdeepseq :: NFData a => Strategy a a
rdeepseq x = do rseq (rnf x); return x

-- Proof of rdeepseq == evalSeq Control.Seq.rdeepseq
--
--    evalSeq Control.Seq.rdeepseq
-- == \x -> Control.Seq.rdeepseq x `pseq` return x
-- == \x -> (x `deepseq` Control.Seq.Done) `pseq` return x
-- == \x -> (rnf x `seq` Control.Seq.Done) `pseq` return x
-- == \x -> rnf x `pseq` return x
-- == rdeepseq

-- | 'rpar' sparks its argument (for evaluation in parallel).
rpar :: Strategy a a
#if __GLASGOW_HASKELL__ >= 702
rpar  x = Eval $ IO $ \s -> spark# x s
#else
rpar  x = case (par# x) of { _ -> Done x }
#endif
{-# INLINE rpar  #-}

-- | Perform a computation in parallel using a strategy.
--
-- @
-- rparWith strat x
-- @
--
-- will spark @strat x@. Note that @rparWith strat@ is /not/ the
-- same as @rpar `dot` strat@. Specifically, @rpar `dot` strat@
-- always sparks a computation to produce a value in WHNF, while
-- @rparWith strat@ need not.
--
-- > rparWith r0 = r0
-- > rparWith rpar = rpar
-- > rparWith rseq = rpar
rparWith :: Strategy a b -> Strategy a b
-- The intermediate `Lift` box is necessary, in order to avoid a built-in
-- `seq` in `rparWith`. In particular, we want rparWith r0 = r0, not
-- rparWith r0 = rpar.
rparWith s a = do
  l <- rpar r
  return (case l of Lift x -> x)

  where
    r = runEval (Lift <$> s a)

data Lift a = Lift a

-- --------------------------------------------------------------------------
-- Strategy combinators for Traversable data types

-- | Evaluate the elements of a traversable data structure
-- according to the given strategy.
evalTraversable :: Traversable t => Strategy a b -> Strategy (t a) (t b)
evalTraversable = traverse
{-# INLINE evalTraversable #-}

-- | Like 'evalTraversable' but evaluates all elements in parallel.
parTraversable :: Traversable t => Strategy a b -> Strategy (t a) (t b)
parTraversable strat = evalTraversable (rparWith strat)
{-# INLINE parTraversable #-}

-- --------------------------------------------------------------------------
-- Strategies for lists

-- | Evaluate each element of a list according to the given strategy.
--  Equivalent to 'evalTraversable' at the list type.
evalList :: Strategy a b -> Strategy [a] [b]
evalList = evalTraversable
-- Alternative explicitly recursive definition:
-- evalList strat []     = return []
-- evalList strat (x:xs) = strat x >>= \x' ->
--                         evalList strat xs >>= \xs' ->
--                         return (x':xs')

-- | Evaluate each element of a list in parallel according to given strategy.
--  Equivalent to 'parTraversable' at the list type.
parList :: Strategy a b -> Strategy [a] [b]
parList = parTraversable
-- Alternative definition via evalList:
-- parList strat = evalList (rparWith strat)

-- | Divides a list into chunks, and applies the strategy
-- @'evalList' strat@ to each chunk in parallel.
--
-- It is expected that this function will be replaced by a more
-- generic clustering infrastructure in the future.
--
-- If the chunk size is 1 or less, 'parListChunk' is equivalent to
-- 'parList'
--
parListChunk :: Int -> Strategy a b -> Strategy [a] [b]
parListChunk n strat xs
  | n <= 1    = parList strat xs
  | otherwise = concat `fmap` parList (evalList strat) (chunk n xs)

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = as : chunk n bs where (as,bs) = splitAt n xs

-- --------------------------------------------------------------------------
-- Convenience

-- | A combination of 'parList' and 'map', encapsulating a common pattern:
--
-- > parMap strat f = withStrategy (parList strat) . map f
-- > parMap strat f = withStrategy (parList (strat . f))
parMap :: Strategy b c -> (a -> b) -> [a] -> [c]
parMap strat f = withStrategy (parList (strat . f))

-- --------------------------------------------------------------------------
-- Strategies for lazy lists

-- List-based non-compositional rolling buffer strategy, evaluating list
-- elements to weak head normal form.
-- Not to be exported; used in evalBuffer and for optimisation.
evalBufferWHNF :: Int -> Strategy [a] [a]
evalBufferWHNF n0 xs0 = return (ret xs0 (start n0 xs0))
  where -- ret :: [a] -> [a] -> [a]
           ret (x:xs) (y:ys) = y `pseq` (x : ret xs ys)
           ret xs     _      = xs

        -- start :: Int -> [a] -> [a]
           start 0   ys     = ys
           start !_n []     = []
           start !n  (y:ys) = y `pseq` start (n-1) ys

-- | 'evalBuffer' is a rolling buffer strategy combinator for (lazy) lists.
--
-- 'evalBuffer' is not as compositional as the type suggests. In fact,
-- it evaluates list elements at least to weak head normal form,
-- disregarding a strategy argument 'r0'.
--
-- > evalBuffer n r0 == evalBuffer n rseq
--
evalBuffer :: Int -> Strategy a b -> Strategy [a] [b]
evalBuffer n strat =  evalBufferWHNF n . map (withStrategy strat)

-- Like evalBufferWHNF but sparks the list elements when pushing them
-- into the buffer.
-- Not to be exported; used in parBuffer and for optimisation.
parBufferWHNF :: Int -> Strategy [a] [a]
parBufferWHNF n0 xs0 = return (ret xs0 (start n0 xs0))
  where -- ret :: [a] -> [a] -> [a]
           ret (x:xs) (y:ys) = y `par` (x : ret xs ys)
           ret xs     _      = xs

        -- start :: Int -> [a] -> [a]
           start 0   ys     = ys
           start !_n []     = []
           start !n  (y:ys) = y `par` start (n-1) ys


-- | Like 'evalBuffer' but evaluates the list elements in parallel when
-- pushing them into the buffer.
parBuffer :: Int -> Strategy a b -> Strategy [a] [b]
parBuffer n strat = parBufferWHNF n . map (withStrategy strat)
-- Alternative definition via evalBuffer (may compromise firing of RULES):
-- parBuffer n strat = evalBuffer n (rparWith strat)

-- Deforest the intermediate list in parBuffer/evalBuffer when it is
-- unnecessary:

{-# NOINLINE [1] evalBuffer #-}
{-# NOINLINE [1] parBuffer #-}
{-# RULES
"evalBuffer/rseq"  forall n . evalBuffer  n rseq = evalBufferWHNF n
"parBuffer/rseq"   forall n . parBuffer   n rseq = parBufferWHNF  n
 #-}

-- --------------------------------------------------------------------------
-- Strategies for tuples

evalTuple2 :: Strategy a a' -> Strategy b b' -> Strategy (a,b) (a',b')
evalTuple2 strat1 strat2 (x1,x2) =
  pure (,) <*> strat1 x1 <*> strat2 x2

evalTuple3 :: Strategy a a'
           -> Strategy b b'
           -> Strategy c c'
           -> Strategy (a,b,c) (a',b',c')
evalTuple3 strat1 strat2 strat3 (x1,x2,x3) =
  pure (,,) <*> strat1 x1 <*> strat2 x2 <*> strat3 x3

evalTuple4 :: Strategy a a'
           -> Strategy b b'
           -> Strategy c c'
           -> Strategy d d'
           -> Strategy (a,b,c,d) (a',b',c',d')
evalTuple4 strat1 strat2 strat3 strat4 (x1,x2,x3,x4) =
  pure (,,,) <*> strat1 x1 <*> strat2 x2 <*> strat3 x3 <*> strat4 x4

evalTuple5 :: Strategy a a'
           -> Strategy b b'
           -> Strategy c c'
           -> Strategy d d'
           -> Strategy e e'
           -> Strategy (a,b,c,d,e) (a',b',c',d',e')
evalTuple5 strat1 strat2 strat3 strat4 strat5 (x1,x2,x3,x4,x5) =
  pure (,,,,) <*> strat1 x1 <*> strat2 x2 <*> strat3 x3 <*> strat4 x4 <*> strat5 x5

evalTuple6 :: Strategy a a'
           -> Strategy b b'
           -> Strategy c c'
           -> Strategy d d'
           -> Strategy e e'
           -> Strategy f f'
           -> Strategy (a,b,c,d,e,f) (a',b',c',d',e',f')
evalTuple6 strat1 strat2 strat3 strat4 strat5 strat6 (x1,x2,x3,x4,x5,x6) =
  pure (,,,,,) <*> strat1 x1 <*> strat2 x2 <*> strat3 x3 <*> strat4 x4 <*> strat5 x5 <*> strat6 x6

evalTuple7 :: Strategy a a'
           -> Strategy b b'
           -> Strategy c c'
           -> Strategy d d'
           -> Strategy e e'
           -> Strategy f f'
           -> Strategy g g'
           -> Strategy (a,b,c,d,e,f,g) (a',b',c',d',e',f',g')
evalTuple7 strat1 strat2 strat3 strat4 strat5 strat6 strat7 (x1,x2,x3,x4,x5,x6,x7) =
  pure (,,,,,,) <*> strat1 x1 <*> strat2 x2 <*> strat3 x3 <*> strat4 x4 <*> strat5 x5 <*> strat6 x6 <*> strat7 x7

evalTuple8 :: Strategy a a'
           -> Strategy b b'
           -> Strategy c c'
           -> Strategy d d'
           -> Strategy e e'
           -> Strategy f f'
           -> Strategy g g'
           -> Strategy h h'
           -> Strategy (a,b,c,d,e,f,g,h) (a',b',c',d',e',f',g',h')
evalTuple8 strat1 strat2 strat3 strat4 strat5 strat6 strat7 strat8 (x1,x2,x3,x4,x5,x6,x7,x8) =
  pure (,,,,,,,) <*> strat1 x1 <*> strat2 x2 <*> strat3 x3 <*> strat4 x4 <*> strat5 x5 <*> strat6 x6 <*> strat7 x7 <*> strat8 x8

evalTuple9 :: Strategy a a'
           -> Strategy b b'
           -> Strategy c c'
           -> Strategy d d'
           -> Strategy e e'
           -> Strategy f f'
           -> Strategy g g'
           -> Strategy h h'
           -> Strategy i i'
           -> Strategy (a,b,c,d,e,f,g,h,i) (a',b',c',d',e',f',g',h',i')
evalTuple9 strat1 strat2 strat3 strat4 strat5 strat6 strat7 strat8 strat9 (x1,x2,x3,x4,x5,x6,x7,x8,x9) =
  pure (,,,,,,,,) <*> strat1 x1 <*> strat2 x2 <*> strat3 x3 <*> strat4 x4 <*> strat5 x5 <*> strat6 x6 <*> strat7 x7 <*> strat8 x8 <*> strat9 x9

parTuple2 :: Strategy a a' -> Strategy b b' -> Strategy (a,b) (a',b')
parTuple2 strat1 strat2 =
  evalTuple2 (rparWith strat1) (rparWith strat2)

parTuple3 :: Strategy a a' -> Strategy b b' -> Strategy c c' -> Strategy (a,b,c) (a',b',c')
parTuple3 strat1 strat2 strat3 =
  evalTuple3 (rparWith strat1) (rparWith strat2) (rparWith strat3)

parTuple4 :: Strategy a a' -> Strategy b b' -> Strategy c c' -> Strategy d d' -> Strategy (a,b,c,d) (a',b',c',d')
parTuple4 strat1 strat2 strat3 strat4 =
  evalTuple4 (rparWith strat1) (rparWith strat2) (rparWith strat3) (rparWith strat4)

parTuple5 :: Strategy a a' -> Strategy b b' -> Strategy c c' -> Strategy d d' -> Strategy e e' -> Strategy (a,b,c,d,e) (a',b',c',d',e')
parTuple5 strat1 strat2 strat3 strat4 strat5 =
  evalTuple5 (rparWith strat1) (rparWith strat2) (rparWith strat3) (rparWith strat4) (rparWith strat5)

parTuple6 :: Strategy a a' -> Strategy b b' -> Strategy c c' -> Strategy d d' -> Strategy e e' -> Strategy f f' -> Strategy (a,b,c,d,e,f) (a',b',c',d',e',f')
parTuple6 strat1 strat2 strat3 strat4 strat5 strat6 =
  evalTuple6 (rparWith strat1) (rparWith strat2) (rparWith strat3) (rparWith strat4) (rparWith strat5) (rparWith strat6)

parTuple7 :: Strategy a a' -> Strategy b b' -> Strategy c c' -> Strategy d d' -> Strategy e e' -> Strategy f f' -> Strategy g g' -> Strategy (a,b,c,d,e,f,g) (a',b',c',d',e',f',g')
parTuple7 strat1 strat2 strat3 strat4 strat5 strat6 strat7 =
  evalTuple7 (rparWith strat1) (rparWith strat2) (rparWith strat3) (rparWith strat4) (rparWith strat5) (rparWith strat6) (rparWith strat7)

parTuple8 :: Strategy a a' -> Strategy b b' -> Strategy c c' -> Strategy d d' -> Strategy e e' -> Strategy f f' -> Strategy g g' -> Strategy h h' -> Strategy (a,b,c,d,e,f,g,h) (a',b',c',d',e',f',g',h')
parTuple8 strat1 strat2 strat3 strat4 strat5 strat6 strat7 strat8 =
  evalTuple8 (rparWith strat1) (rparWith strat2) (rparWith strat3) (rparWith strat4) (rparWith strat5) (rparWith strat6) (rparWith strat7) (rparWith strat8)

parTuple9 :: Strategy a a' -> Strategy b b' -> Strategy c c' -> Strategy d d' -> Strategy e e' -> Strategy f f' -> Strategy g g' -> Strategy h h' -> Strategy i i' -> Strategy (a,b,c,d,e,f,g,h,i) (a',b',c',d',e',f',g',h',i')
parTuple9 strat1 strat2 strat3 strat4 strat5 strat6 strat7 strat8 strat9 =
  evalTuple9 (rparWith strat1) (rparWith strat2) (rparWith strat3) (rparWith strat4) (rparWith strat5) (rparWith strat6) (rparWith strat7) (rparWith strat8) (rparWith strat9)

-- --------------------------------------------------------------------------
-- Strategic function application

{-
These are very handy when writing pipeline parallelism as a sequence of
@$@, @$|@ and @$||@'s. There is no need of naming intermediate values
in this case. The separation of algorithm from strategy is achieved by
allowing strategies only as second arguments to @$|@ and @$||@.

Note: unlike the similarly-named operators in "Control.Parallel.Strategies",
these do /not/ necessarily evaluate the results of strategy application
to WHNF; rather, evaluation is limited to that performed by the passed
strategy.
-}

-- | Sequential function application. The argument is evaluated using
--   the given strategy before it is given to the function.
($|) :: (b -> c) -> Strategy a b -> a -> c
f $| s  = \ x -> runEval (f <$> s x)

-- | Parallel function application. The argument is evaluated using
-- the given strategy, in parallel with the function application.
($||) :: (b -> c) -> Strategy a b -> a -> c
f $|| s = \ x -> runEval (f <$> rparWith s x)

-- | Sequential function composition. The result of
-- the second function is evaluated using the given strategy,
-- and then given to the first function.
(.|) :: (c -> d) -> Strategy b c -> (a -> b) -> (a -> d)
(.|) f s g = (f $| s) . g

-- | Parallel function composition. The result of the second
-- function is evaluated using the given strategy,
-- in parallel with the application of the first function.
(.||) :: (c -> d) -> Strategy b c -> (a -> b) -> (a -> d)
(.||) f s g = (f $|| s) . g

-- | Sequential inverse function composition,
-- for those who read their programs from left to right.
-- The result of the first function is evaluated using the
-- given strategy, and then given to the second function.
(-|) :: (a -> b) -> Strategy b c -> (c -> d) -> (a -> d)
(-|) f s g = (.|) g s f

-- | Parallel inverse function composition,
-- for those who read their programs from left to right.
-- The result of the first function is evaluated using the
-- given strategy, in parallel with the application of the
-- second function.
(-||) :: (a -> b) -> Strategy b c -> (c -> d) -> (a -> d)
(-||) f s g = (.||) g s f
