`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/coreSyn/MkCore.hs>`_

Note [Flattening one-tuples]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This family of functions creates a tuple of variables/expressions/types.
  mkCoreTup [e1,e2,e3] = (e1,e2,e3)
What if there is just one variable/expression/type in the argument?
We could do one of two things:

* Flatten it out, so that
    mkCoreTup [e1] = e1

* Build a one-tuple (see Note [One-tuples] in TysWiredIn)
    mkCoreTup1 [e1] = Unit e1
  We use a suffix "1" to indicate this.

Usually we want the former, but occasionally the latter.


Note [aBSENT_SUM_FIELD_ERROR_ID]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Absent argument error for unused unboxed sum fields are different than absent
error used in dummy worker functions (see `mkAbsentErrorApp`):

- `absentSumFieldError` can't take arguments because it's used in unarise for
  unused pointer fields in unboxed sums, and applying an argument would
  require allocating a thunk.

- `absentSumFieldError` can't be CAFFY because that would mean making some
  non-CAFFY definitions that use unboxed sums CAFFY in unarise.

  To make `absentSumFieldError` non-CAFFY we get a stable pointer to it in
  RtsStartup.c and mark it as non-CAFFY here.

Getting this wrong causes hard-to-debug runtime issues, see #15038.

TODO: Remove stable pointer hack after fixing #9718.
      However, we should still be careful about not making things CAFFY just
      because they use unboxed sums. Unboxed objects are supposed to be
      efficient, and none of the other unboxed literals make things CAFFY.


Note [Error and friends have an "open-tyvar" forall]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
'error' and 'undefined' have types
        error     :: forall (v :: RuntimeRep) (a :: TYPE v). String -> a
        undefined :: forall (v :: RuntimeRep) (a :: TYPE v). a
Notice the runtime-representation polymorphism. This ensures that
"error" can be instantiated at unboxed as well as boxed types.
This is OK because it never returns, so the return type is irrelevant.




Note [aBSENT_ERROR_ID]
~~~~~~~~~~~~~~~~~~~~~~
We use aBSENT_ERROR_ID to build dummy values in workers.  E.g.

   f x = (case x of (a,b) -> b) + 1::Int

The demand analyser figures ot that only the second component of x is
used, and does a w/w split thus

   f x = case x of (a,b) -> $wf b

   $wf b = let a = absentError "blah"
               x = (a,b)
           in <the original RHS of f>

After some simplification, the (absentError "blah") thunk goes away.

------ Tricky wrinkle -------
#14285 had, roughly

   data T a = MkT a !a
   {-# INLINABLE f #-}
   f x = case x of MkT a b -> g (MkT b a)

It turned out that g didn't use the second component, and hence f doesn't use
the first.  But the stable-unfolding for f looks like
   \x. case x of MkT a b -> g ($WMkT b a)
where $WMkT is the wrapper for MkT that evaluates its arguments.  We
apply the same w/w split to this unfolding (see Note [Worker-wrapper
for INLINEABLE functions] in WorkWrap) so the template ends up like
   \b. let a = absentError "blah"
           x = MkT a b
        in case x of MkT a b -> g ($WMkT b a)

After doing case-of-known-constructor, and expanding $WMkT we get
   \b -> g (case absentError "blah" of a -> MkT b a)

Yikes!  That bogusly appears to evaluate the absentError!

This is extremely tiresome.  Another way to think of this is that, in
Core, it is an invariant that a strict data contructor, like MkT, must
be applied only to an argument in HNF. So (absentError "blah") had
better be non-bottom.

So the "solution" is to add a special case for absentError to exprIsHNFlike.
This allows Simplify.rebuildCase, in the Note [Case to let transformation]
branch, to convert the case on absentError into a let. We also make
absentError *not* be diverging, unlike the other error-ids, so that we
can be sure not to remove the case branches before converting the case to
a let.

If, by some bug or bizarre happenstance, we ever call absentError, we should
throw an exception.  This should never happen, of course, but we definitely
can't return anything.  e.g. if somehow we had
    case absentError "foo" of
       Nothing -> ...
       Just x  -> ...
then if we return, the case expression will select a field and continue.
Seg fault city. Better to throw an exception. (Even though we've said
it is in HNF :-)

It might seem a bit surprising that seq on absentError is simply erased

    absentError "foo" `seq` x ==> x

but that should be okay; since there's no pattern match we can't really
be relying on anything from it.

