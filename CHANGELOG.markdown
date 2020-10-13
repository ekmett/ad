next [????.??.??]
-----------------
* Change the fixity of `:-` in `Numeric.AD.Jet` to be right-associative.
  Previously, it was `infixl`, which made things like `x :- y :- z` nearly
  unusable.
* Fix backpropagation error in Kahn mode.
* Fix bugs in the `Erf` instance for `ForwardDouble`.
* Add `Numeric.AD.Mode.Reverse.Double`, a variant of `Numeric.AD.Mode.Reverse`
  that is specialized to `Double`.
* Re-export `Jet(..)`, `headJet`, `tailJet` and `jet` from `Numeric.AD`.

4.4 [2020.02.03]
----------------
* Generalize the type of `stochasticGradientDescent`:

  ```diff
  -stochasticGradientDescent :: (Traversable f, Fractional a, Ord a) => (forall s. Reifies s Tape => f (Scalar a) -> f (Reverse s a) -> Reverse s a) -> [f (Scalar a)] -> f a -> [f a]
  +stochasticGradientDescent :: (Traversable f, Fractional a, Ord a) => (forall s. Reifies s Tape => e            -> f (Reverse s a) -> Reverse s a) -> [e]            -> f a -> [f a]
  ```

4.3.6 [2019.02.28]
------------------
* Make the test suite pass when built against `musl` `libc`.

4.3.5 [2018.01.18]
------------------
* Add `Semigroup` instance for `Id`.

4.3.4
-----
* Support `doctest-0.12`

4.3.3
-----
* Revamp `Setup.hs` to use `cabal-doctest`. This makes it build
  with `Cabal-2.0`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.

4.3.2.1
-----
* GHC 8 support
* Fix Kahn mode's `**` implementation
* Fix multiple problems in Erf and InvErf methods

4.3.2
-----
* Added `NoEq` versions of several combinators that can be used when `Eq` isn't available on the numeric type involved.

4.3.1
-----
* Further improvements have been made in the performance of `Sparse` mode, at least asymptotically, when used on functions with many variables.
  Since this is the target use-case for `Sparse` in the first place, this seems like a good trade-off. Note: this results in an API change, but
  only in the API of an `Internal` module, so this is treated as a minor version bump.

4.3
---
* Made drastic improvements in the performance of `Tower` and `Sparse` modes thanks to the help of Björn von Sydow.
* Added constrained convex optimization.
* Incorporated some suggestions from [herbie](http://herbie.uwplse.org/z) for improving floating point accuracy.

4.2.4
-----
* Added `Newton.Double` modules for performance.

4.2.3
-----
* `reflection` 2 support

4.2.2
-----
* *Major* bug fix for `grads`, `jacobians`, and anything that uses `Sparse` mode in `Numeric.AD`. Derivatives after the first two
  were previously incorrect.

4.2.1.1
-------
* Support `nats` version 1

4.2.1
-----
* Added `stochasticGradientDescent`.

4.2
---
* Removed broken `Directed` mode.
* Added `Numeric.AD.Rank1` combinators and moved most infinitesimal handling back out of the modes and into an `AD` wrapper.

4.1
---
* Fixed a bug in the type of `conjugateGradientAscent` and `conjugateGradientDescent` that prevent users from being able to ever call it.

4.0.0.1
-------
* Added the missing `instances.h` header file to `extra-source-files`.

4.0
---
* An overhaul permitting monomorphic modes was completed by @alang9.
* Add a `ForwardDouble` monomorphic mode

3.4
---
* Added support for `erf` and `inverf`, etc. from `Data.Number.Erf`.
* Split the infinitesimal and mode into two separate parameters to facilitate inlining and easier extension of the API.

3.3.1
-----
* Build system improvements
* Removed unused LANGUAGE pragmas
* Added HLint configuration
* We now use exactly the same versions of the packages used to build `ad` when running the doctests.

3.3
---
* Renamed `Reverse` to `Kahn` and `Wengert` to `Reverse`. We use Arthur Kahn's topological sorting algorithm to
  sort the tape after the fact in Kahn mode, while the stock Reverse mode builds a Wengert list as it goes, which
  is more efficient in practice.

3.2.2
-----
* Export of the `conjugateGradientDescent` and `gradientDescent` from `Numeric.AD`

3.2.1
---
* `conjugateGradientDescent` now stops before it starts returning NaN results.

3.2
---
* Renamed `Chain` to `Wengert` to reflect its use of Wengert lists for reverse mode.
* Renamed `lift` to `auto` to avoid conflict with the more prevalent `transformers` library.
* Fixed a bug in `Numeric.AD.Forward.gradWith'`, which caused it to return the wrong value for the primal.

3.1.4
-----
* Added a better "convergence" test for `findZero`
* Compute `tan` and `tanh` derivatives directly.

3.1.3
-----
* Added `conjugateGradientDescent` and `conjugateGradientAscent` to `Numeric.AD.Newton`.

3.1.2
-----
* Dependency bump

3.1
---
* Added `Chain` mode, which is `Reverse` using a linear tape that doesn't need to be sorted.
* Added a suite of doctests.
* Bug fix in `Forward` mode. It was previously yielding incorrect results for anything that used `bind` or `bind'` internally.

3.0
---
* Moved the contents of `Numeric.AD.Mode.Mixed` into `Numeric.AD`
* Split off `Numeric.AD.Variadic` for the variadic combinators
* Removed the `UU`, `FU`, `UF`, and `FF` type aliases.
* Stopped exporting the types for `Mode` and `AD` from almost every module. Import `Numeric.AD.Types` if necessary.
* Renamed `Tensors` to `Jet`
* Dependency bump to be compatible with ghc 7.4.1 and mtl 2.1
* More aggressive zero tracking.
* `diff (**n) 0` for constant n and `diff (0**)` both now yield the correct answer for all modes.
