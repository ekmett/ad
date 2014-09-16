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
