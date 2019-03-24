`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnBinds.hs>`_

Note [Pattern bindings that bind no variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Generally, we want to warn about pattern bindings like
  Just _ = e
because they don't do anything!  But we have three exceptions:

* A wildcard pattern
       _ = rhs
  which (a) is not that different from  _v = rhs
        (b) is sometimes used to give a type sig for,
            or an occurrence of, a variable on the RHS

* A strict pattern binding; that is, one with an outermost bang
     !Just _ = e
  This can fail, so unlike the lazy variant, it is not a no-op.
  Moreover, #13646 argues that even for single constructor
  types, you might want to write the constructor.  See also #9127.

* A splice pattern
      $(th-lhs) = rhs
   It is impossible to determine whether or not th-lhs really
   binds any variable. We should disable the warning for any pattern
   which contain splices, but that is a more expensive check.



Note [Free-variable space leak]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We have
    fvs' = trim fvs
and we seq fvs' before turning it as part of a record.

The reason is that trim is sometimes something like
    \xs -> intersectNameSet (mkNameSet bound_names) xs
and we don't want to retain the list bound_names. This showed up in
trac ticket #1136.


Note [Renaming pattern synonym variables]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We rename pattern synonym declaractions backwards to normal to reuse
the logic already implemented for renaming patterns.

We first rename the RHS of a declaration which brings into
scope the variables bound by the pattern (as they would be
in normal function definitions). We then lookup the variables
which we want to bind in this local environment.

It is crucial that we then only lookup in the *local* environment which
only contains the variables brought into scope by the pattern and nothing
else. Amazingly no-one encountered this bug for 3 GHC versions but
it was possible to define a pattern synonym which referenced global
identifiers and worked correctly.

```
x = 5

pattern P :: Int -> ()
pattern P x <- _

f (P x) = x

> f () = 5
```

See #13470 for the original report.



Note [Pattern synonym builders don't yield dependencies]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When renaming a pattern synonym that has an explicit builder,
references in the builder definition should not be used when
calculating dependencies. For example, consider the following pattern
synonym definition:

pattern P x <- C1 x where
  P x = f (C1 x)

f (P x) = C2 x

In this case, 'P' needs to be typechecked in two passes:

1. Typecheck the pattern definition of 'P', which fully determines the
   type of 'P'. This step doesn't require knowing anything about 'f',
   since the builder definition is not looked at.

2. Typecheck the builder definition, which needs the typechecked
   definition of 'f' to be in scope; done by calls oo tcPatSynBuilderBind
   in TcBinds.tcValBinds.

This behaviour is implemented in 'tcValBinds', but it crucially
depends on 'P' not being put in a recursive group with 'f' (which
would make it look like a recursive pattern synonym a la 'pattern P =
P' which is unsound and rejected).

So:
 * We do not include builder fvs in the Uses returned by rnPatSynBind
   (which is then used for dependency analysis)
 * But we /do/ include them in the psb_fvs for the PatSynBind
 * In rnValBinds we record these builder uses, to avoid bogus
   unused-variable warnings (#12548)


Note [Orphan COMPLETE pragmas]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We define a COMPLETE pragma to be a non-orphan if it includes at least
one conlike defined in the current module. Why is this sufficient?
Well if you have a pattern match

.. code-block:: haskell

  case expr of
    P1 -> ...
    P2 -> ...
    P3 -> ...

any COMPLETE pragma which mentions a conlike other than P1, P2 or P3
will not be of any use in verifying that the pattern match is
exhaustive. So as we have certainly read the interface files that
define P1, P2 and P3, we will have loaded all non-orphan COMPLETE
pragmas that could be relevant to this pattern match.

For now we simply disallow orphan COMPLETE pragmas, as the added
complexity of supporting them properly doesn't seem worthwhile.

