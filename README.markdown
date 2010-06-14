ad
==

A package that provides an intuitive API for [Automatic Differentiation](http://en.wikipedia.org/wiki/Automatic_differentiation) (AD) in Haskell. Automatic differentiation provides a means to calculate the derivatives of a function while evaluating it. Unlike numerical methods based on running the program with multiple inputs or symbolic approaches, automatic differentiation typically only decreases performance by a small multiplier.

AD employs the fact that any program `y = F(x)` that computes one or more value does so by composing multiple primitive operations. If the (partial) derivatives of each of those operations is known, then they can be composed to derive the answer for the derivative of the entire program at a point.

This library contains at its core a single implementation that describes how to compute the partial derivatives of a wide array of primitive operations. It then exposes an API that enables a user to safely combine them using standard higher-order functions, just as you would with any other Haskell numerical type.

There are several ways to compose these individual [Jacobian matrices](http://en.wikipedia.org/wiki/Jacobian_matrix_and_determinant). We hide the choice used by the API behind an explicit "Mode" type-class and universal quantification. This prevents the end user from exploiting the properties of an individual mode, and thereby potentially violating invariants or [confusing infinitesimals](http://conway.rutgers.edu/~ccshan/wiki/blog/posts/Differentiation/).

Features
--------

 * Provides forward- and reverse- mode AD combinators with a common API.
 * Type-level "branding" is used to both prevent the end user from confusing infinitesimals and to limit unsafe access to the implementation details of each mode.
 * Each mode has a separate module full of combinators, with a consistent look and feel.

Examples
---------

You can compute derivatives of functions 

    Prelude Numeric.AD> diff sin 0 {-# cos 0 #-}
    1.0

Or both the answer and the derivative of a function:

    Prelude Numeric.AD> diff' (exp . log) 2
    (2.0,1.0)

You can use a symbolic numeric type, like the one from `simple-reflect` to obtain symbolic derivatives:

    Prelude Debug.SimpleReflect Numeric.AD> diff atanh x
    recip (1 - x * x) * 1

You can compute gradients for functions that take non-scalar values in the form of a Traversable functor full of AD variables.

    Prelude Numeric.AD Debug.SimpleReflect> grad (\[x,y,z] -> x * sin (x + log y)) [x,y,z]
    [ 0 + (0 + sin (x + log y) * 1 + 1 * (0 + cos (x + log y) * (0 + x * 1)))
    , 0 + (0 + recip y * (0 + 1 * (0 + cos (x + log y) * (0 + x * 1))))
    , 0 
    ]

which one can simplify to:

    [ sin (x + log y) + cos (x + log y) * x, recip y * cos (x + log y) * x, 0 ]

If you need multiple derivatives you can calculate them with `diffs`:

    Prelude Numeric.AD> take 10 $ diffs sin 1
    [0.8414709848078965,0.5403023058681398,-0.8414709848078965,-0.5403023058681398,0.8414709848078965,0.5403023058681398,-0.8414709848078965,-0.5403023058681398,0.8414709848078965,0.5403023058681398]

or if your function takes multiple inputs, you can use grads, which returns an 'f-branching stream' of derivatives. Somewhat more intuitive answers can be obtained by converting the stream into the 
polymorphically recursive `Tensors` data type. With that we can look at a single 'layer' of the answer at a time:

The answer:
   
    Prelude Numeric.AD> headT $ tensors $  grads (\[x,y] -> exp (x * y)) [1,2]
    7.38905609893065

The gradient:

    Prelude Numeric.AD> headT $ tailT $ tensors $  grads (\[x,y] -> exp (x * y)) [1,2]
    [14.7781121978613,7.38905609893065]

The hessian (n * n matrix of 2nd derivatives)

    Prelude Numeric.AD> headT $ tailT $ tailT $ tensors $  grads (\[x,y] -> exp (x * y)) [1,2]
    [[29.5562243957226,22.16716829679195],[22.16716829679195,7.38905609893065]]

Or even higher order tensors of derivatives.

    Prelude Numeric.AD> headT $ tailT $ tailT $ tailT $ tensors $  grads (\[x,y] -> exp (x * y)) [1,2]
    [[[59.1124487914452,44.3343365935839],[44.3343365935839,14.7781121978613]],[[44.3343365935839,14.7781121978613],[14.7781121978613,7.38905609893065]]]

Note the redundant values caused by the various symmetries in the tensors. The 'ad' library is careful to compute each distinct derivative only once and to share the resulting thunks.

Overview
--------

### Modules

 * `Numeric.AD.Mode.Forward` provides basic forward-mode AD. It is good for computing simple derivatives.
 * `Numeric.AD.Mode.Sparse` computes a sparse forward-mode AD tower. It is good for higher derivatives or large numbers of outputs.
 * `Numeric.AD.Mode.Tower` computes a dense forward-mode AD tower useful for higher derivatives of single input functions.
 * `Numeric.AD.Mode.Mixed` computes using whichever mode or combination thereof is suitable to each individual combinator. This mode is the default, re-exported by `Numeric.AD`

### Combinators

While not every mode can provide all operations, the following basic operations are supported, modified as appropriate by the suffixes below:

 * `grad` computes the gradient (vector of partial derivatives at a given point) of a function.
 * `jacobian` computes the Jacobian matrix of a function at a point.
 * `diff` computes the derivative of a function at a point.
 * `du` computes a directional derivative of a function at a point.
 * `hessian` computes the Hessian matrix (matrix of second partial derivatives) of a function at a point.

### Combinator Sufes

The following suffixes alter the meanings of the functions above as follows:
 * `\'` -- also return the answer
 * `With` lets the user supply a function to blend the input with the output
 * `F` is a version of the base function lifted to return a `Traversable` (or `Functor`) result
 * `s` means the function returns all higher derivatives in a list or f-branching `Stream`
 * `T` means the result is transposed with respect to the traditional formulation.
 * `0` means that the resulting derivative list is padded with 0s at the end.

Contact Information
-------------------

Contributions and bug reports are welcome!

Please feel free to contact me through github or on the #haskell IRC channel on irc.freenode.net.

-Edward Kmett
