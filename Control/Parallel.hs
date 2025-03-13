{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Parallel
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  stable
-- Portability :  portable
--
-- Parallel constructs.
--
-- A common pattern to evaluate two values in parallel is:
--
-- > x `par` y `pseq` someFunc x y
--
-- The effect of this pattern is to cause @x@ to be evaluated in
-- parallel with @y@. When the evaluation of @y@ is complete, computation
-- proceeds by evaluating @someFunc x y@.
--
-----------------------------------------------------------------------------

module Control.Parallel (
          par, pseq
    ) where

#ifdef __GLASGOW_HASKELL__
import qualified GHC.Conc (par, pseq)

infixr 0 `par`, `pseq`
#endif

-- Maybe parIO and the like could be added here later.

-- | Indicates that it may be beneficial to evaluate the first
-- argument in parallel with the second.  Returns the value of the
-- second argument.
--
-- The result of @a ``par`` b@ is always  @b@, regardless of wether
-- @a@ evaluates to a bottom, so for example @par undefined x = x@.
--
-- @par@ is generally used when the value of @a@ is likely to be
-- required later, but not immediately.  Also it is a good idea to
-- ensure that @a@ is not a trivial computation, otherwise the cost of
-- spawning it in parallel overshadows the benefits obtained by
-- running it in parallel.
--
-- Note that actual parallelism is only supported by certain
-- implementations (GHC with the @-threaded@ option, for now).
-- On other implementations, @par a b = b@.
par :: a -> b -> b
#ifdef __GLASGOW_HASKELL__
par = GHC.Conc.par
#else
-- For now, Hugs does not support par properly.
par a b = b
#endif

-- | Like 'seq' but ensures that the first argument is evaluated before returning.
--
-- @a ``pseq`` b@ evaluates @a@ to weak head normal form (WHNF)
-- before returning @b@.
--
-- This is similar to 'seq', but with a subtle difference:
-- 'seq' is strict in both its arguments, so the compiler
-- may, for example, rearrange @a ``seq`` b@ into @b ``seq`` a ``seq`` b@.
-- This is normally no problem when using 'seq' to express strictness,
-- but it can be a problem when annotating code for parallelism,
-- because we need more control over the order of evaluation; we may
-- want to evaluate @a@ before @b@, because we know that @b@ has
-- already been sparked in parallel with 'par'.
--
-- This is why we have 'pseq'.  In contrast to 'seq', 'pseq' is only
-- strict in its first argument (as far as the compiler is concerned),
-- which restricts the transformations that the compiler can do, and
-- ensures that the user can retain control of the evaluation order.
pseq :: a -> b -> b
#ifdef __GLASGOW_HASKELL__
pseq = GHC.Conc.pseq
#else
pseq = seq
#endif
