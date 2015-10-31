# Changelog for [`parallel` package](http://hackage.haskell.org/package/parallel)

## 3.2.0.7  *TBA*

  - Add `{-# NOINLINE[1] rseq #-}` to make the `RULE` more robust
  - Make `rpar`'s type signature consistent with `rseq` & co

## 3.2.0.6  *Dec 2014*

  - Make `-Wall` message free for all supported `base` versions

## 3.2.0.5  *Dec 2014*

  - Support `base-4.8.0.0`/`deepseq-1.4.0.0` (and thus GHC 7.10)

## 3.2.0.4  *Nov 2013*

  * Update package description to Cabal 1.10 format
  * Add support for GHC 7.8
  * Drop support for GHCs older than GHC 7.0.1
  * Add NOINLINE pragmas to `parBuffer`, `parList`, and `evalBuffer`
    to make RULEs more likely to fire
