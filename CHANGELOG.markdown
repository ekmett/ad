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
