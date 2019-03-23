Note [Deep skolemisation]
~~~~~~~~~~~~~~~~~~~~~~~~~
deeplySkolemise decomposes and skolemises a type, returning a type
with all its arrows visible (ie not buried under foralls)

Examples:

  deeplySkolemise (Int -> forall a. Ord a => blah)
    =  ( wp, [a], [d:Ord a], Int -> blah )
    where wp = \x:Int. /\a. \(d:Ord a). <hole> x

  deeplySkolemise  (forall a. Ord a => Maybe a -> forall b. Eq b => blah)
    =  ( wp, [a,b], [d1:Ord a,d2:Eq b], Maybe a -> blah )
    where wp = /\a.\(d1:Ord a).\(x:Maybe a)./\b.\(d2:Ord b). <hole> x

In general,
  if      deeplySkolemise ty = (wrap, tvs, evs, rho)
    and   e :: rho
  then    wrap e :: ty
    and   'wrap' binds tvs, evs

ToDo: this eta-abstraction plays fast and loose with termination,
      because it can introduce extra lambdas.  Maybe add a `seq` to
      fix this


Note [Handling boxed equality]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The solver deals entirely in terms of unboxed (primitive) equality.
There should never be a boxed Wanted equality. Ever. But, what if
we are calling `foo :: forall a. (F a ~ Bool) => ...`? That equality
is boxed, so naive treatment here would emit a boxed Wanted equality.

So we simply check for this case and make the right boxing of evidence.

--------------


Note [Signature files and type class instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Instances in signature files do not have an effect when compiling:
when you compile a signature against an implementation, you will
see the instances WHETHER OR NOT the instance is declared in
the file (this is because the signatures go in the EPS and we
can't filter them out easily.)  This is also why we cannot
place the instance in the hi file: it would show up as a duplicate,
and we don't have instance reexports anyway.

However, you might find them useful when typechecking against
a signature: the instance is a way of indicating to GHC that
some instance exists, in case downstream code uses it.

Implementing this is a little tricky.  Consider the following
situation (sigof03):

 module A where
     instance C T where ...

 module ASig where
     instance C T

When compiling ASig, A.hi is loaded, which brings its instances
into the EPS.  When we process the instance declaration in ASig,
we should ignore it for the purpose of doing a duplicate check,
since it's not actually a duplicate. But don't skip the check
entirely, we still want this to fail (tcfail221):

 module ASig where
     instance C T
     instance C T

Note that in some situations, the interface containing the type
class instances may not have been loaded yet at all.  The usual
situation when A imports another module which provides the
instances (sigof02m):

 module A(module B) where
     import B

See also Note [Signature lazy interface loading].  We can't
rely on this, however, since sometimes we'll have spurious
type class instances in the EPS, see #9422 (sigof02dm)

