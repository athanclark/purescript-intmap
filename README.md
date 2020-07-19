purescript-intmap
===============

## Why?

Although we have [ordered-collections](https://pursuit.purescript.org/packages/purescript-ordered-collections),
a `Map Int a` introduces many levels of indirection; it's internally based on a binary/ternary tree for performing
lookups, and although very efficient for _arbitrary_ key types that support total ordering, is not efficient for
a simple type like `Int`, which could capitalize on bitwise efficiency.

However, we also have `Foreign.Object`, which is just a javascript map, but _acts_ like a `Map String a` - acting
as fast as javascript permits when performing lookups a' la `foo["bar"]`.

This package is not intended to be performant with respect to Haskell's infamous `IntMap` in `https://hackage.haskell.org/package/containers`;
there is no amortized cost analysis or Patricia trees here; just a simple wrapper around `Foreign.Object`.

However, it is convenient for those of us who _want_ to use numbers as keys, but don't want to do all the
parsing and printing manually.
