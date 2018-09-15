[[src]](https://github.com/ghc/ghc/tree/master/compiler/utils/BooleanFormula.hs)
# LANGUAGE DeriveDataTypeable, DeriveFunctor, DeriveFoldable,
             DeriveTraversable #

### Note: Simplification of BooleanFormulas

The smart constructors (`mkAnd` and `mkOr`) do some attempt to simplify expressions. In particular,
 1. Collapsing nested ands and ors, so
     `(mkAnd [x, And [y,z]]`
    is represented as
     `And [x,y,z]`
    Implemented by `fromAnd`/`fromOr`
 2. Collapsing trivial ands and ors, so
     `mkAnd [x]` becomes just `x`.
    Implemented by mkAnd' / mkOr'
 3. Conjunction with false, disjunction with true is simplified, i.e.
     `mkAnd [mkFalse,x]` becomes `mkFalse`.
 4. Common subexpression elimination:
     `mkAnd [x,x,y]` is reduced to just `mkAnd [x,y]`.

This simplification is not exhaustive, in the sense that it will not produce
the smallest possible equivalent expression. For example,
`Or [And [x,y], And [x]]` could be simplified to `And [x]`, but it currently
is not. A general simplifier would need to use something like BDDs.