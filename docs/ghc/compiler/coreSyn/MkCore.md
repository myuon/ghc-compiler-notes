[[src]](https://github.com/ghc/ghc/tree/master/compiler/coreSyn/MkCore.hs)
# \subsection{Basic CoreSyn construction}



The functions from this point don't really do anything cleverer than
their counterparts in CoreSyn, but they are here for consistency


# \subsection{Making literals}


# \subsection{Tuple constructors}



Creating tuples and their types for Core expressions

@mkBigCoreVarTup@ builds a tuple; the inverse to @mkTupleSelector@.

* If it has only one element, it is the identity function.

* If there are more elements than a big tuple can have, it nests
  the tuples.

### Note: Flattening one-tuples

This family of functions creates a tuple of variables/expressions/types.
  mkCoreTup [e1,e2,e3] = (e1,e2,e3)
What if there is just one variable/expression/type in the argument?
We could do one of two things:

* Flatten it out, so that
    mkCoreTup [e1] = e1

### Note: One-tuples

Usually we want the former, but occasionally the latter.


# \subsection{Tuple destructors}


# Floats


# \subsection{Common list manipulation expressions}


Call the constructor Ids when building explicit lists, so that they
interact well with rules.


# Manipulating Maybe data type


# Error expressions


# Error Ids


GHC randomly injects these into the code.

@patError@ is just a version of @error@ for pattern-matching
failures.  It knows various ``codes'' which expand to longer
strings---this saves space!

@absentErr@ is a thing we put in for ``absent'' arguments.  They jolly
well shouldn't be yanked on, but if one is, then you will get a
friendly message from @absentErr@ (rather than a totally random
crash).

@parError@ is a special version of @error@ which the compiler does
not know to be a bottoming Id.  It is used in the @_par_@ and @_seq_@
templates, but we don't ever expect to generate code for it.


### Note: Error and friends have an "open-tyvar" forall

'error' and 'undefined' have types
        error     :: forall (v :: RuntimeRep) (a :: TYPE v). String -> a
        undefined :: forall (v :: RuntimeRep) (a :: TYPE v). a
Notice the runtime-representation polymorphism. This ensures that
"error" can be instantiated at unboxed as well as boxed types.
This is OK because it never returns, so the return type is irrelevant.

# aBSENT_ERROR_ID


### Note: aBSENT_ERROR_ID

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
Trac #14285 had, roughly