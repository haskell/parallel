-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Parallel.Strategies
-- Copyright   :  (c) The University of Glasgow 2001-2009
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Parallel Evaluation Strategies, or Strategies for short, specify a
-- way to evaluate a structure with components in sequence or in
-- parallel.
--
-- Strategies are for expressing /deterministic parallelism/:
-- the result of the program is unaffected by evaluating in parallel.
-- For non-deterministic parallel programming, see
-- "Control.Concurrent".
-- 
-- Strategies let you separate the description of parallelism from the
-- logic of your program, enabling modular parallelism.
--
-- Version 1.x
--
--   The original Strategies design is described in
--      <http://www.macs.hw.ac.uk/~dsg/gph/papers/html/Strategies/strategies.html>
--   and the code was written by
--	Phil Trinder, Hans-Wolfgang Loidl, Kevin Hammond et al. 
--
-- Version 2.x
--
-- Later, during work on the shared-memory implementation of
-- parallelism in GHC, we discovered that the original formulation of
-- Strategies had some problems, in particular it lead to space leaks
-- and difficulties expressing speculative parallelism.  Details are in
-- the paper \"Runtime Support for Multicore Haskell\" <http://www.macs.hw.ac.uk/~dsg/gph/papers/html/Strategies/strategies.html>.
--
-- The "Control.Parallel.Strategies" module has been rewritten in
-- version 2.
--
-- The main change is to the 'Strategy a' type synonym, which was
-- previously @a -> Done@ and is now @a -> a@.  This change helps
-- to fix the space leak described in \"Runtime Support for
-- Multicore Haskell\".  The problem is that the runtime will
-- currently retain the memory referenced by all sparks, until
-- they are evaluated.  Hence, we must arrange to evaluate all the
-- sparks eventually, just in case they aren't evaluated in
-- parallel, so that they don't cause a space leak.  This is why
-- we must return a \"new\" value after applying a 'Strategy', so
-- that the application can evaluate each spark created by the
-- 'Strategy'.
-- 
-- The simple rule is this: you /must/ use the result of applying
-- a 'Strategy' if the strategy creates parallel sparks, and you
-- should probably discard the the original value.  If you don't
-- do this, currently it may result in a space leak.  In the
-- future (GHC 6.14), it will probably result in lost parallelism
-- instead, as we plan to change GHC so that unreferenced sparks
-- are discarded rather than retained (we can't make this change
-- until most code is switched over to this new version of
-- Strategies, because code using the old verison of Strategies
-- would be broken by the change in policy).
--
-- The other changes in version 2.x are:
--
--   * Strategies can now be defined using a convenient Applicative
--     type Eval.  e.g. parList s = unEval $ traverse (Par . s)
--
--   * 'parList' has been generalised to 'parTraverse', which works on
--     any 'Traversable' type.
--
--   * 'parList' and 'parBuffer' have versions specialised to 'rwhnf',
--     and there are transformation rules that automatically translate
--     e.g. @parList rwnhf@ into a call to the optimised version.
--
--   * NFData has been moved into a package of its own, @deepseq@,
--     and renamed to 'DeepSeq'.  The 'rnf' function is still exported
--     by 'Control.Parallel.Strategies'.

-----------------------------------------------------------------------------

module Control.Parallel.Strategies (
    -- * Strategy type and basic operations
    Strategy,
    using,
    withStrategy,
    rwhnf, rnf,
    -- * Tuple strategies
    seqPair, parPair,
    seqTriple, parTriple,
    -- * General traversals
    seqTraverse,
    parTraverse,
    -- * List strategies
    parList, seqList,
    parListN, parListChunk,
    parMap,
    parBuffer,
    -- * Simple list strategies
    parListWHNF,
    parBufferWHNF,
    -- * Strategy composition operators
   ($|), ($||),
   (.|), (.||),
   (-|), (-||),
    -- * Building strategies
    Eval(..), unEval,

    -- * Deprecated functionality
    Done, demanding, sparking, (>|), (>||), r0, 
  ) where

import Data.Traversable
import Control.Applicative
import Control.Parallel
import Control.DeepSeq

-- -----------------------------------------------------------------------------
-- Eval

-- | `Eval` is an Applicative Functor that makes it easier to define
-- parallel strategies that involve traversing structures.
--
-- a 'Seq' value will be evaluated strictly in sequence in its context,
-- whereas a 'Par' value wraps an expression that may be evaluated in
-- parallel.  The Applicative instance allows sequential composition,
-- making it possible to describe an evaluateion strategy by composing
-- 'Par' and 'Seq' with '<*>'.
--
-- For example,
--
-- > parList :: Strategy a -> Strategy [a]
-- > parList strat = unEval . traverse (Par . strat)
--
-- > seqPair :: Strategy a -> Strategy b -> Strategy (a,b)
-- > seqPair f g (a,b) = unEval $ (,) <$> Seq (f a) <*> Seq (g b)
--
data Eval a = Seq a | Par a | Lazy a

unEval :: Eval a -> a
unEval (Seq  a) = a
unEval (Par  a) = a
unEval (Lazy a) = a

instance Functor Eval where
  fmap f (Par   a) = Seq   (a `par`  f a) -- only Par once: context becomes Seq
  fmap f (Seq   a) = Seq   (a `pseq` f a)
  fmap f (Lazy  a) = Lazy  (f a)

instance Applicative Eval where
  pure a = Lazy a
  Par f  <*> x = f `par`  fmap f x
  Seq f  <*> x = f `pseq` fmap f x
  Lazy f <*> x = fmap f x

-- -----------------------------------------------------------------------------
-- Strategies

-- | A 'Strategy' is a function that embodies a parallel evaluation strategy.
-- The function traverses (parts of) its argument, evaluating subexpressions
-- in parallel or in sequence.
-- 
-- A 'Strategy' may do an arbitrary amount of evaluation of its
-- argument, but should not return a value different from the one it
-- was passed.
--
-- Parallel computations may be discarded by the runtime system if the
-- program no longer requires their result, which is why a 'Strategy'
-- function returns a new value equivalent to the old value.  The
-- intention is that the program applies the 'Strategy' to a
-- structure, and then uses the returned value, discarding the old
-- value.  This idiom is expressed by the 'using' function.
-- 
type Strategy a = a -> a

-- | evaluate a value using the given 'Strategy'.
--
-- > using x s = s x
--
using :: a -> Strategy a -> a
using x s = s x

-- | evaluate a value using the given 'Strategy'.  This is simply
-- 'using' with the arguments reversed, and is equal to '($)'.
-- 
withStrategy :: Strategy a -> a -> a
withStrategy = ($)

-- | A 'Strategy' that simply evaluates its argument to Weak Head Normal
-- Form (i.e. evaluates it as far as the topmost constructor).
rwhnf :: Strategy a
rwhnf a = a

-- | A 'Strategy' that fully evaluates its argument
-- 
-- > rnf a = deepseq a `pseq` a
--
rnf :: DeepSeq a => Strategy a
rnf a = deepseq a `pseq` a

-- -----------------------------------------------------------------------------
-- Tuples

seqPair :: Strategy a -> Strategy b -> Strategy (a,b)
seqPair f g (a,b) = unEval $
  (,) <$> Seq (f a) <*> Seq (g b)

parPair :: Strategy a -> Strategy b -> Strategy (a,b)
parPair f g (a,b) = unEval $
  (,) <$> Par (f a) <*> Par (g b)

seqTriple :: Strategy a -> Strategy b -> Strategy c -> Strategy (a,b,c)
seqTriple f g h (a,b,c) = unEval $
  (,,) <$> Seq (f a) <*> Seq (g b) <*> Seq (h c)

parTriple :: Strategy a -> Strategy b -> Strategy c -> Strategy (a,b,c)
parTriple f g h (a,b,c) = unEval $
  (,,) <$> Par (f a) <*> Par (g b) <*> Par (h c)

-- -----------------------------------------------------------------------------
-- General sequential/parallel traversals

-- | A strategy that traverses a container data type with an instance
-- of 'Traversable', and sparks each of the elements using the supplied
-- strategy.
parTraverse :: Traversable t => Strategy a -> Strategy (t a)
parTraverse strat = unEval . traverse (Par . strat)

-- | A strategy that traverses a container data type with an instance
-- of 'Traversable', and evaluates each of the elements in left-to-right
-- sequence using the supplied strategy.
seqTraverse :: Traversable t => Strategy a -> Strategy (t a)
seqTraverse strat = unEval . traverse (Seq . strat)

{-# SPECIALISE parTraverse :: Strategy a -> Strategy [a] #-}
{-# SPECIALISE seqTraverse :: Strategy a -> Strategy [a] #-}

-- -----------------------------------------------------------------------------
-- Lists

-- | Spark each of the elements of a list using the given strategy.
-- Equivalent to 'parTraverse' at the list type.
parList :: Strategy a -> Strategy [a]
parList = parTraverse

-- | Evaluate each of the elements of a list sequentially from left to right
-- using the given strategy.  Equivalent to 'seqTraverse' at the list type.
seqList :: Strategy a -> Strategy [a]
seqList = seqTraverse

parListN :: Int -> Strategy a -> Strategy [a]
parListN n strat xs = parList strat as ++ bs where (as,bs) = splitAt n xs

parListChunk :: Int -> Strategy a -> Strategy [a]
parListChunk n strat xs = concat (parList (seqList strat) (chunk n xs))

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = as : chunk n bs where (as,bs) = splitAt n xs

parMap :: Strategy b -> (a -> b) -> [a] -> [b]
parMap strat f = parList strat . map f

-- -----------------------------------------------------------------------------
-- parBuffer

-- | Applies a strategy to the nth element of list when the head is demanded.
-- More precisely:
--
-- * semantics: @parBuffer n s = id :: [a] -> [a]@
--
-- * dynamic behaviour: evalutates the nth element of the list when the
-- head is demanded.
--
-- The idea is to provide a `rolling buffer' of length n.  It is a
-- better than 'parList' for a lazy stream, because 'parList' will
-- evaluate the entire list, whereas 'parBuffer' will only evaluate a
-- fixed number of elements ahead.

parBuffer :: Int -> Strategy a -> [a] -> [a]
parBuffer n strat xs = parBufferWHNF n (map strat xs)

-- -----------------------------------------------------------------------------
-- Simple strategies

-- These are non-compositional strategies that might be more efficient
-- than their more general counterparts.  We use RULES to do the
-- specialisation.

{-# RULES 
"parList/rwhnf" parList rwhnf = parListWHNF
"parList/id"    parList id    = parListWHNF
"parBuffer/rwhnf" forall n . parBuffer n rwhnf = parBufferWHNF n
"parBuffer/id"    forall n . parBuffer n id    = parBufferWHNF n
 #-}

-- | version of 'parList' specialised to 'rwhnf'.  This version is
-- much simpler, and may be faster than 'parList rwhnf'.  You should
-- never need to use this directly, since 'parList rwhnf' is
-- automatically optimised to 'parListWHNF'.  It is here for
-- experimentation purposes only.
parListWHNF :: Strategy [a]
parListWHNF []     = []
parListWHNF (x:xs) = x `par` parListWHNF xs

-- | version of 'parBuffer' specialised to 'rwhnf'.  You should
-- never need to use this directly, since 'parBuffer rwhnf' is
-- automatically optimised to 'parBufferWHNF'.  It is here for
-- experimentation purposes only.
parBufferWHNF :: Int -> Strategy [a]
parBufferWHNF n0 xs0 = ret xs0 (start n0 xs0)
  where
    ret (x:xs) (y:ys) = y `par` (x : return xs ys)
    ret xs     _      = xs

    start _ []     = []
    start 0 ys     = ys
    start n (y:ys) = y `par` start (n-1) ys

------------------------------------------------------------------------------
-- *                     Strategic Function Application
------------------------------------------------------------------------------

{-
These are very
handy when writing pipeline parallelism asa sequence of @$@, @$|@ and
@$||@'s. There is no need of naming intermediate values in this case. The
separation of algorithm from strategy is achieved by allowing strategies
only as second arguments to @$|@ and @$||@.
-}

-- | Sequential function application. The argument is evaluated using
--   the given strategy before it is given to the function.
($|) :: (a -> b) -> Strategy a -> a -> b
f $| s  = \ x -> let z = x `using` s in z `pseq` f z

-- | Parallel function application. The argument is evaluated using
-- the given strategy, in parallel with the function application.
($||) :: (a -> b) -> Strategy a -> a -> b
f $|| s = \ x -> let z = x `using` s in z `par` f z

-- | Sequential function composition. The result of
-- the second function is evaluated using the given strategy, 
-- and then given to the first function.
(.|) :: (b -> c) -> Strategy b -> (a -> b) -> (a -> c)
(.|) f s g = \ x -> let z = s (g x) in 
                    z `pseq` f z

-- | Parallel function composition. The result of the second
-- function is evaluated using the given strategy,
-- in parallel with the application of the first function.
(.||) :: (b -> c) -> Strategy b -> (a -> b) -> (a -> c)
(.||) f s g = \ x -> let z = s (g x) in 
                    z `par` f z

-- | Sequential inverse function composition, 
-- for those who read their programs from left to right.
-- The result of the first function is evaluated using the 
-- given strategy, and then given to the second function.
(-|) :: (a -> b) -> Strategy b -> (b -> c) -> (a -> c)
(-|) f s g = \ x -> let z = s (f x) in 
                    z `pseq` g z

-- | Parallel inverse function composition,
-- for those who read their programs from left to right.
-- The result of the first function is evaluated using the 
-- given strategy, in parallel with the application of the 
-- second function.
(-||) :: (a -> b) -> Strategy b -> (b -> c) -> (a -> c)
(-||) f s g = \ x -> let z = s (f x) in 
                    z `par` g z

-- -----------------------------------------------------------------------------
-- Old/deprecated stuff

{-# DEPRECATED Done "The Strategies type is now a -> a, not a -> Done" #-}
type Done = ()

{-# DEPRECATED demanding "Use pseq or $| instead" #-}
demanding :: a -> Done -> a
demanding = flip pseq

{-# DEPRECATED sparking "Use par or $|| instead" #-}
sparking :: a -> Done -> a
sparking  = flip par

{-# DEPRECATED (>|) "Use pseq or $| instead" #-}
(>|) :: Done -> Done -> Done 
(>|) = Prelude.seq

{-# DEPRECATED (>||) "Use par or $|| instead" #-}
(>||) :: Done -> Done -> Done 
(>||) = par

{-# DEPRECATED r0 "Strategies must return a result, there is no r0 any more" #-}
r0 :: a -> ()
r0 _ = ()
