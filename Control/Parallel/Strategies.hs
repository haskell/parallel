{-# LANGUAGE BangPatterns, CPP, MagicHash, UnboxedTuples #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Parallel.Strategies
-- Copyright   :  (c) The University of Glasgow 2001-2010
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Parallel Evaluation Strategies, or Strategies for short, provide
-- ways to express parallel computations.  Strategies have the following
-- key features:
--
--  * Strategies express /deterministic parallelism/:
--    the result of the program is unaffected by evaluating in parallel.
--    The parallel tasks evaluated by a Strategy may have no side effects.
--    For non-deterministic parallel programming, see "Control.Concurrent".
--
--  * Strategies let you separate the description of the parallelism from the
--    logic of your program, enabling modular parallelism.  The basic idea
--    is to build a lazy data structure representing the computation, and
--    then write a Strategy that describes how to traverse the data structure
--    and evaluate components of it sequentially or in parallel.
--
--  * Strategies are /compositional/: larger strategies can be built
--    by gluing together smaller ones.
--
--  * 'Monad' and 'Applicative' instances are provided, for quickly building
--    strategies that involve traversing structures in a regular way.
--
-- For API history and changes in this release, see "Control.Parallel.Strategies#history".

-----------------------------------------------------------------------------

module Control.Parallel.Strategies (
         -- * The strategy type
         Strategy

         -- * Application of strategies
       , using             -- :: a -> Strategy a -> a
       , withStrategy      -- :: Strategy a -> a -> a
       , usingIO           -- :: a -> Strategy a -> IO a
       , withStrategyIO    -- :: Strategy a -> a -> IO a

         -- * Composition of strategies
       , dot               -- :: Strategy a -> Strategy a -> Strategy a

         -- * Basic strategies
       , r0                -- :: Strategy a
       , rseq
       , rdeepseq          -- :: NFData a => Strategy a
       , rpar              -- :: Strategy a
       , rparWith          -- :: Strategy a -> Strategy a

         -- * Injection of sequential strategies
       , evalSeq           -- :: Seq.Strategy a -> Strategy a
       , SeqStrategy

         -- * Strategies for traversable data types
       , evalTraversable   -- :: Traversable t => Strategy a -> Strategy (t a)
       , parTraversable

         -- * Strategies for lists
       , evalList          -- :: Strategy a -> Strategy [a]
       , parList
       , evalListN         -- :: Int -> Strategy a -> Strategy [a]
       , parListN
       , evalListNth       -- :: Int -> Strategy a -> Strategy [a]
       , parListNth
       , evalListSplitAt   -- :: Int -> Strategy [a] -> Strategy [a] -> Strategy [a]
       , parListSplitAt
       , parListChunk
       , parMap

         -- ** Strategies for lazy lists
       , evalBuffer        -- :: Int -> Strategy a -> Strategy [a]
       , parBuffer

         -- * Strategies for tuples

         -- | Evaluate the components of a tuple according to the
         -- given strategies.

       , evalTuple2        -- :: Strategy a -> ... -> Strategy (a,...)
       , evalTuple3
       , evalTuple4
       , evalTuple5
       , evalTuple6
       , evalTuple7
       , evalTuple8
       , evalTuple9


       -- | Evaluate the components of a tuple in parallel according to
       -- the given strategies.

       , parTuple2         -- :: Strategy a -> ... -> Strategy (a,...)
       , parTuple3
       , parTuple4
       , parTuple5
       , parTuple6
       , parTuple7
       , parTuple8
       , parTuple9

         -- * Strategic function application
       , ($|)              -- :: (a -> b) -> Strategy a -> a -> b
       , ($||)
       , (.|)              -- :: (b -> c) -> Strategy b -> (a -> b) -> a -> c
       , (.||)
       , (-|)              -- :: (a -> b) -> Strategy b -> (b -> c) -> a -> c
       , (-||)

         -- * For Strategy programmers
       , Eval              -- instances: Monad, Functor, Applicative
       , runEval           -- :: Eval a -> a
       , runEvalIO         -- :: Eval a -> IO a
       ,

    -- * API History

    -- $history

    -- * Backwards compatibility

    -- | These functions and types are all deprecated, and will be
    -- removed in a future release.  In all cases they have been
    -- either renamed or replaced with equivalent functionality.

    Done, demanding, sparking, (>|), (>||),
    rwhnf, unEval,
    seqTraverse, parTraverse,
    seqList,
    seqPair, parPair,
    seqTriple, parTriple,

    -- * For API completeness

    -- | so users of 'rdeepseq' aren't required to import Control.DeepSeq:
    NFData
  ) where

#if !MIN_VERSION_base(4,8,0)
import Data.Traversable
import Control.Applicative
#endif
import Control.Parallel
import Control.DeepSeq (NFData)

import qualified Control.Seq
import qualified Control.Parallel.Strategies.Poly as P
import Control.Parallel.Strategies.Poly (Eval, runEval, runEvalIO)

infixr 9 `dot`     -- same as (.)
infixl 0 `using`   -- lowest precedence and associate to the left
infixl 0 `usingIO` -- lowest precedence and associate to the left

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
type Strategy a = a -> Eval a

-- | Evaluate a value using the given 'Strategy'.
--
-- > x `using` s = runEval (s x)
--
using :: a -> Strategy a -> a
using = P.using

-- | evaluate a value using the given 'Strategy'.  This is simply
-- 'using' with the arguments reversed.
--
withStrategy :: Strategy a -> a -> a
withStrategy = P.withStrategy

-- | Evaluate a value using the given 'Strategy' inside the 'IO' monad.  See
-- also 'runEvalIO'.
--
-- > x `usingIO` s = runEvalIO (s x)
--
usingIO :: a -> Strategy a -> IO a
usingIO = P.usingIO

-- | Evaluate a value using the given 'Strategy' inside the 'IO' monad.  This
-- is simply 'usingIO' with the arguments reversed.
--
withStrategyIO :: Strategy a -> a -> IO a
withStrategyIO = P.withStrategyIO

-- | Compose two strategies sequentially.
-- This is the analogue to function composition on strategies.
--
-- For any strategies @strat1@, @strat2@, and @strat3@,
--
-- > (strat1 `dot` strat2) `dot` strat3 == strat1 `dot` (strat2 `dot` strat3)
-- > strat1 `dot` strat1 = strat1
-- > strat1 `dot` r0 == strat1
--
-- > strat2 `dot` strat1 == strat2 . withStrategy strat1
--
dot :: Strategy a -> Strategy a -> Strategy a
dot = P.dot

-- | Inject a sequential strategy (ie. coerce a sequential strategy
-- to a general strategy).
--
-- Thanks to 'evalSeq', the type @Control.Seq.Strategy a@ is a subtype
-- of @'Strategy' a@.
evalSeq :: SeqStrategy a -> Strategy a
evalSeq = P.evalSeq

-- | A name for @Control.Seq.Strategy@, for documentation only.
type SeqStrategy a = Control.Seq.Strategy a

-- --------------------------------------------------------------------------
-- Basic strategies (some imported from SeqStrategies)

-- | 'r0' performs *no* evaluation.
--
-- > r0 == evalSeq Control.Seq.r0
--
r0 :: Strategy a
r0 = P.r0

-- | 'rseq' evaluates its argument to weak head normal form.
--
-- > rseq == evalSeq Control.Seq.rseq
--
rseq :: Strategy a
rseq = P.rseq

-- | 'rdeepseq' fully evaluates its argument.
--
-- > rdeepseq == evalSeq Control.Seq.rdeepseq
--
rdeepseq :: NFData a => Strategy a
rdeepseq = P.rdeepseq

-- | 'rpar' sparks its argument (for evaluation in parallel).
rpar :: Strategy a
rpar = P.rpar

-- | Perform a computation in parallel using a strategy.
--
-- @
-- rparWith strat x
-- @
--
-- will spark @strat x@. Note that @rparWith strat@ is /not/ the
-- same as @rpar `dot` strat@. Specifically, @rpar `dot` strat@
-- always produces a value in WHNF, while @rparWith strat@ need
-- not.
--
-- > rparWith r0 = r0
-- > rparWith rpar = rpar
-- > rparWith rseq = rpar
rparWith :: Strategy a -> Strategy a
rparWith = P.rparWith

-- --------------------------------------------------------------------------
-- Strategy combinators for Traversable data types

-- | Evaluate the elements of a traversable data structure
-- according to the given strategy.
evalTraversable :: Traversable t => Strategy a -> Strategy (t a)
evalTraversable = traverse
{-# INLINE evalTraversable #-}

-- | Like 'evalTraversable' but evaluates all elements in parallel.
parTraversable :: Traversable t => Strategy a -> Strategy (t a)
parTraversable = P.parTraversable

-- --------------------------------------------------------------------------
-- Strategies for lists

-- | Evaluate each element of a list according to the given strategy.
--  Equivalent to 'evalTraversable' at the list type.
evalList :: Strategy a -> Strategy [a]
evalList = evalTraversable

-- | Evaluate each element of a list in parallel according to given strategy.
--  Equivalent to 'parTraversable' at the list type.
parList :: Strategy a -> Strategy [a]
parList = parTraversable

-- | @'evaListSplitAt' n stratPref stratSuff@ evaluates the prefix
-- (of length @n@) of a list according to @stratPref@ and its the suffix
-- according to @stratSuff@.
evalListSplitAt :: Int -> Strategy [a] -> Strategy [a] -> Strategy [a]
evalListSplitAt n stratPref stratSuff xs
  = let (ys,zs) = splitAt n xs in
    stratPref ys >>= \ys' ->
    stratSuff zs >>= \zs' ->
    return (ys' ++ zs')

-- | Like 'evalListSplitAt' but evaluates both sublists in parallel.
parListSplitAt :: Int -> Strategy [a] -> Strategy [a] -> Strategy [a]
parListSplitAt n stratPref stratSuff = evalListSplitAt n (rparWith stratPref) (rparWith stratSuff)

-- | Evaluate the first n elements of a list according to the given strategy.
evalListN :: Int -> Strategy a -> Strategy [a]
evalListN n strat = evalListSplitAt n (evalList strat) r0

-- | Like 'evalListN' but evaluates the first n elements in parallel.
parListN :: Int -> Strategy a -> Strategy [a]
parListN n strat = evalListN n (rparWith strat)

-- | Evaluate the nth element of a list (if there is such) according to
-- the given strategy.
-- This nth is 0-based. For example, @[1, 2, 3, 4, 5] `using` evalListNth 4 rseq@
-- will eval @5@, not @4@.
-- The spine of the list up to the nth element is evaluated as a side effect.
evalListNth :: Int -> Strategy a -> Strategy [a]
evalListNth n strat = evalListSplitAt n r0 (evalListN 1 strat)

-- | Like 'evalListN' but evaluates the nth element in parallel.
parListNth :: Int -> Strategy a -> Strategy [a]
parListNth n strat = evalListNth n (rparWith strat)

-- | Divides a list into chunks, and applies the strategy
-- @'evalList' strat@ to each chunk in parallel.
--
-- It is expected that this function will be replaced by a more
-- generic clustering infrastructure in the future.
--
-- If the chunk size is 1 or less, 'parListChunk' is equivalent to
-- 'parList'
--
parListChunk :: Int -> Strategy a -> Strategy [a]
parListChunk = P.parListChunk

-- --------------------------------------------------------------------------
-- Convenience

-- | A combination of 'parList' and 'map', encapsulating a common pattern:
--
-- > parMap strat f = withStrategy (parList strat) . map f
--
parMap :: Strategy b -> (a -> b) -> [a] -> [b]
parMap = P.parMap

-- --------------------------------------------------------------------------
-- Strategies for lazy lists

-- | 'evalBuffer' is a rolling buffer strategy combinator for (lazy) lists.
--
-- 'evalBuffer' is not as compositional as the type suggests. In fact,
-- it evaluates list elements at least to weak head normal form,
-- disregarding a strategy argument 'r0'.
--
-- > evalBuffer n r0 == evalBuffer n rseq
--
evalBuffer :: Int -> Strategy a -> Strategy [a]
evalBuffer = P.evalBuffer

-- | Like 'evalBuffer' but evaluates the list elements in parallel when
-- pushing them into the buffer.
parBuffer :: Int -> Strategy a -> Strategy [a]
parBuffer = P.parBuffer

-- --------------------------------------------------------------------------
-- Strategies for tuples

evalTuple2 :: Strategy a -> Strategy b -> Strategy (a,b)
evalTuple2 = P.evalTuple2

evalTuple3 :: Strategy a -> Strategy b -> Strategy c -> Strategy (a,b,c)
evalTuple3 = P.evalTuple3

evalTuple4 :: Strategy a -> Strategy b -> Strategy c -> Strategy d -> Strategy (a,b,c,d)
evalTuple4 = P.evalTuple4

evalTuple5 :: Strategy a -> Strategy b -> Strategy c -> Strategy d -> Strategy e -> Strategy (a,b,c,d,e)
evalTuple5 = P.evalTuple5

evalTuple6 :: Strategy a -> Strategy b -> Strategy c -> Strategy d -> Strategy e -> Strategy f -> Strategy (a,b,c,d,e,f)
evalTuple6 = P.evalTuple6

evalTuple7 :: Strategy a -> Strategy b -> Strategy c -> Strategy d -> Strategy e -> Strategy f -> Strategy g -> Strategy (a,b,c,d,e,f,g)
evalTuple7 = P.evalTuple7

evalTuple8 :: Strategy a -> Strategy b -> Strategy c -> Strategy d -> Strategy e -> Strategy f -> Strategy g -> Strategy h -> Strategy (a,b,c,d,e,f,g,h)
evalTuple8 = P.evalTuple8

evalTuple9 :: Strategy a -> Strategy b -> Strategy c -> Strategy d -> Strategy e -> Strategy f -> Strategy g -> Strategy h -> Strategy i -> Strategy (a,b,c,d,e,f,g,h,i)
evalTuple9 = P.evalTuple9

parTuple2 :: Strategy a -> Strategy b -> Strategy (a,b)
parTuple2 = P.parTuple2

parTuple3 :: Strategy a -> Strategy b -> Strategy c -> Strategy (a,b,c)
parTuple3 = P.parTuple3

parTuple4 :: Strategy a -> Strategy b -> Strategy c -> Strategy d -> Strategy (a,b,c,d)
parTuple4 = P.parTuple4

parTuple5 :: Strategy a -> Strategy b -> Strategy c -> Strategy d -> Strategy e -> Strategy (a,b,c,d,e)
parTuple5 = P.parTuple5

parTuple6 :: Strategy a -> Strategy b -> Strategy c -> Strategy d -> Strategy e -> Strategy f -> Strategy (a,b,c,d,e,f)
parTuple6 = P.parTuple6

parTuple7 :: Strategy a -> Strategy b -> Strategy c -> Strategy d -> Strategy e -> Strategy f -> Strategy g -> Strategy (a,b,c,d,e,f,g)
parTuple7 = P.parTuple7

parTuple8 :: Strategy a -> Strategy b -> Strategy c -> Strategy d -> Strategy e -> Strategy f -> Strategy g -> Strategy h -> Strategy (a,b,c,d,e,f,g,h)
parTuple8 = P.parTuple8

parTuple9 :: Strategy a -> Strategy b -> Strategy c -> Strategy d -> Strategy e -> Strategy f -> Strategy g -> Strategy h -> Strategy i -> Strategy (a,b,c,d,e,f,g,h,i)
parTuple9 = P.parTuple9

-- --------------------------------------------------------------------------
-- Strategic function application

{-
These are very handy when writing pipeline parallelism asa sequence of
@$@, @$|@ and @$||@'s. There is no need of naming intermediate values
in this case. The separation of algorithm from strategy is achieved by
allowing strategies only as second arguments to @$|@ and @$||@.
-}

-- NOTE: We don't define these in terms of the operators
-- in Control.Parallel.Strategies.Poly because the latter have somewhat
-- different strictness properties.

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
(.|) f s g = \ x -> let z = g x `using` s in
                    z `pseq` f z

-- | Parallel function composition. The result of the second
-- function is evaluated using the given strategy,
-- in parallel with the application of the first function.
(.||) :: (b -> c) -> Strategy b -> (a -> b) -> (a -> c)
(.||) f s g = \ x -> let z = g x `using` s in
                    z `par` f z

-- | Sequential inverse function composition,
-- for those who read their programs from left to right.
-- The result of the first function is evaluated using the
-- given strategy, and then given to the second function.
(-|) :: (a -> b) -> Strategy b -> (b -> c) -> (a -> c)
(-|) f s g = \ x -> let z = f x `using` s in
                    z `pseq` g z

-- | Parallel inverse function composition,
-- for those who read their programs from left to right.
-- The result of the first function is evaluated using the
-- given strategy, in parallel with the application of the
-- second function.
(-||) :: (a -> b) -> Strategy b -> (b -> c) -> (a -> c)
(-||) f s g = \ x -> let z = f x `using` s in
                    z `par` g z

-- -----------------------------------------------------------------------------
-- Old/deprecated stuff

{-# DEPRECATED Done "The Strategy type is now a -> Eval a, not a -> Done" #-}
-- | DEPRECCATED: replaced by the 'Eval' monad
type Done = ()

{-# DEPRECATED demanding "Use pseq or $| instead" #-}
-- | DEPRECATED: Use 'pseq' or '$|' instead
demanding :: a -> Done -> a
demanding = flip pseq

{-# DEPRECATED sparking "Use par or $|| instead" #-}
-- | DEPRECATED: Use 'par' or '$||' instead
sparking :: a -> Done -> a
sparking  = flip par

{-# DEPRECATED (>|) "Use pseq or $| instead" #-}
-- | DEPRECATED: Use 'pseq' or '$|' instead
(>|) :: Done -> Done -> Done
(>|) = Prelude.seq

{-# DEPRECATED (>||) "Use par or $|| instead" #-}
-- | DEPRECATED: Use 'par' or '$||' instead
(>||) :: Done -> Done -> Done
(>||) = par

{-# DEPRECATED rwhnf "renamed to rseq" #-}
-- | DEPRECATED: renamed to 'rseq'
rwhnf :: Strategy a
rwhnf = rseq

{-# DEPRECATED seqTraverse "renamed to evalTraversable" #-}
-- | DEPRECATED: renamed to 'evalTraversable'
seqTraverse :: Traversable t => Strategy a -> Strategy (t a)
seqTraverse = evalTraversable

{-# DEPRECATED parTraverse "renamed to parTraversable" #-}
-- | DEPRECATED: renamed to 'parTraversable'
parTraverse :: Traversable t => Strategy a -> Strategy (t a)
parTraverse = parTraversable

{-# DEPRECATED seqList "renamed to evalList" #-}
-- | DEPRECATED: renamed to 'evalList'
seqList :: Strategy a -> Strategy [a]
seqList = evalList

{-# DEPRECATED seqPair "renamed to evalTuple2" #-}
-- | DEPRECATED: renamed to 'evalTuple2'
seqPair :: Strategy a -> Strategy b -> Strategy (a,b)
seqPair = evalTuple2

{-# DEPRECATED parPair "renamed to parTuple2" #-}
-- | DEPRECATED: renamed to 'parTuple2'
parPair :: Strategy a -> Strategy b -> Strategy (a,b)
parPair = parTuple2

{-# DEPRECATED seqTriple "renamed to evalTuple3" #-}
-- | DEPRECATED: renamed to 'evalTuple3'
seqTriple :: Strategy a -> Strategy b -> Strategy c -> Strategy (a,b,c)
seqTriple = evalTuple3

{-# DEPRECATED parTriple "renamed to parTuple3" #-}
-- | DEPRECATED: renamed to 'parTuple3'
parTriple :: Strategy a -> Strategy b -> Strategy c -> Strategy (a,b,c)
parTriple = parTuple3

{-# DEPRECATED unEval "renamed to runEval" #-}
-- | DEPRECATED: renamed to 'runEval'
unEval :: Eval a -> a
unEval = runEval

{- $history #history#

The strategies library has a long history.  What follows is a
summary of how the current design evolved, and is mostly of
interest to those who are familiar with an older version, or need
to adapt old code to use the newer API.

Version 1.x

  The original Strategies design is described in /Algorithm + Strategy = Parallelism/ <http://www.macs.hw.ac.uk/~dsg/gph/papers/html/Strategies/strategies.html>
  and the code was written by
     Phil Trinder, Hans-Wolfgang Loidl, Kevin Hammond et al.

Version 2.x

Later, during work on the shared-memory implementation of
parallelism in GHC, we discovered that the original formulation of
Strategies had some problems, in particular it lead to space leaks
and difficulties expressing speculative parallelism.  Details are in
the paper /Runtime Support for Multicore Haskell/ <http://community.haskell.org/~simonmar/papers/multicore-ghc.pdf>.

This module has been rewritten in version 2. The main change is to
the 'Strategy a' type synonym, which was previously @a -> Done@ and
is now @a -> Eval a@.  This change helps to fix the space leak described
in \"Runtime Support for Multicore Haskell\".  The problem is that
the runtime will currently retain the memory referenced by all
sparks, until they are evaluated.  Hence, we must arrange to
evaluate all the sparks eventually, just in case they aren't
evaluated in parallel, so that they don't cause a space leak.  This
is why we must return a \"new\" value after applying a 'Strategy',
so that the application can evaluate each spark created by the
'Strategy'.

The simple rule is this: you /must/ use the result of applying
a 'Strategy' if the strategy creates parallel sparks, and you
should probably discard the the original value.  If you don't
do this, currently it may result in a space leak.  In the
future (GHC 6.14), it will probably result in lost parallelism
instead, as we plan to change GHC so that unreferenced sparks
are discarded rather than retained (we can't make this change
until most code is switched over to this new version of
Strategies, because code using the old verison of Strategies
would be broken by the change in policy).

The other changes in version 2.x are:

  * Strategies can now be defined using a convenient Monad/Applicative
    type, 'Eval'.  e.g. @parList s = traverse (Par . (``using`` s))@

  * 'parList' has been generalised to 'parTraverse', which works on
    any 'Traversable' type, and similarly 'seqList' has been generalised
    to 'seqTraverse'

  * 'parList' and 'parBuffer' have versions specialised to 'rwhnf',
    and there are transformation rules that automatically translate
    e.g. @parList rwnhf@ into a call to the optimised version.

  * 'NFData' has been moved to @Control.DeepSeq@ in the @deepseq@
    package.  Note that since the 'Strategy' type changed, 'rnf'
    is no longer a 'Strategy': use 'rdeepseq' instead.

Version 2.1 moved NFData into a separate package, @deepseq@.

Version 2.2 changed the type of Strategy to @a -> Eval a@, and
re-introduced the @r0@ strategy which was missing in version 2.1.

Version 2.3 simplified the @Eval@ type, so that @Eval@ is now just
the strict identity monad.  This change and various other
improvements and refactorings are thanks to Patrick Maier who
noticed that @Eval@ didn't satisfy the monad laws, and that a
simpler version would fix that problem.

(version 2.3 was not released on Hackage).

Version 3 introduced a major overhaul of the API, to match what is
presented in the paper

  /Seq no More: Better Strategies for Parallel Haskell/
  <http://community.haskell.org/~simonmar/papers/strategies.pdf>

The major differences in the API are:

 * The addition of Sequential strategies ("Control.Seq") as
   a composable means for specifying sequential evaluation.

 * Changes to the naming scheme: 'rwhnf' renamed to 'rseq',
   'seqList' renamed to 'evalList', 'seqPair' renamed to
   'evalTuple2',

The naming scheme is now as follows:

  * Basic polymorphic strategies (of type @'Strategy' a@) are called @r...@.
    Examples: 'r0', 'rseq', 'rpar', 'rdeepseq'.

  * A strategy combinator for a particular type constructor
    or constructor class @T@ is called @evalT...@, @parT...@ or @seqT...@.

  * The @seqT...@ combinators (residing in module
     "Control.Seq") yield sequential strategies.
     Thus, @seqT...@ combinators cannot spark, nor can the sequential
     strategies to which they may be applied.
     Examples: 'seqTuple2', 'seqListN', 'seqFoldable'.

  * The @evalT...@ combinators do not spark themselves, yet they may
     be applied to strategies that do spark. (They may also be applied
     to non-sparking strategies; however, in that case the corresponding
     @seqT...@ combinator might be a better choice.)
     Examples: 'evalTuple2', 'evalListN', 'evalTraversable'.

  * The @parT...@ combinators, which are derived from their @evalT...@
     counterparts, do spark. They may be applied to all strategies,
     whether sparking or not.
     Examples: 'parTuple2', 'parListN', 'parTraversable'.

  * An exception to the type driven naming scheme are 'evalBuffer' and
     'parBuffer', which are not named after their type constructor (lists)
     but after their function (rolling buffer of fixed size).
-}
