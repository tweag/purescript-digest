# PureScript Digestive Computations

Digest cycles are computations that support reading a context value (the
input), accumulating a monoidal view of the context (the view), and possibly
producing a final result (the result). One application of them is to implement
composable forms, where the validation logic and UI are run simultaneously.

The idea is based loosely on the Formlets introduced in (The Essence of Form
Abstraction)[http://www.ezrakilty.net/pubs/formlets.pdf] by Cooper, Lindley,
Wadlerm and Yallop, altough this implementation includes a mechanism for failed
validation as well as the ability to run computations in an arbitrary base
monad via a monad transformer, `DigestT`.

The name `Digest` is taken from the
(digestive-functors)[https://hackage.haskell.org/package/digestive-functors]
Haskell package, though it should be noted that the design of the types and API
are radically different in this library. Notably, the `DigestT` monad does not
impose any structure on the view type beyond requiring a Monoid instance. This
makes the API more general, but less domain-specific. As such, it is an
appropriate choice to build domain-specific form libraries that target
particular frameworks and design systems, allowing it to be tailored to the
specific use case more easily.

## Overview

The library provides the `DigestT` and `Digest` types, the latter of which is
`DigestT` applied to the `Identity` base monad.

A `Digest` action can be thought of combining the abilities of `Reader`,
`Writer`, and `Maybe`. Input can be accessed at any point in the computation,
and modified locally like `Reader`. The view can be appended to at any point in
the computation, as well as manipulated directly like `Writer`. Finally, the
computation can short-circuit and fail with an empty result, yeilding the view
accumulated so far. This ability enables things like form validation and
wizards naturally.
