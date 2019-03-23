`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/rename/RnEnv.hs>`_

Note [Signature lazy interface loading]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GHC's lazy interface loading can be a bit confusing, so this Note is an
empirical description of what happens in one interesting case. When
compiling a signature module against an its implementation, we do NOT
load interface files associated with its names until after the type
checking phase.  For example:

    module ASig where
        data T
        f :: T -> T

Suppose we compile this with -sig-of "A is ASig":

    module B where
        data T = T
        f T = T

    module A(module B) where
        import B

During type checking, we'll load A.hi because we need to know what the
RdrEnv for the module is, but we DO NOT load the interface for B.hi!
It's wholly unnecessary: our local definition 'data T' in ASig is all
the information we need to finish type checking.  This is contrast to
type checking of ordinary Haskell files, in which we would not have the
local definition "data T" and would need to consult B.hi immediately.
(Also, this situation never occurs for hs-boot files, since you're not
allowed to reexport from another module.)

After type checking, we then check that the types we provided are
consistent with the backing implementation (in checkHiBootOrHsigIface).
At this point, B.hi is loaded, because we need something to compare
against.

I discovered this behavior when trying to figure out why type class
instances for Data.Map weren't in the EPS when I was type checking a
test very much like ASig (sigof02dm): the associated interface hadn't
been loaded yet!  (The larger issue is a moot point, since an instance
declared in a signature can never be a duplicate.)

This behavior might change in the future.  Consider this
alternate module B:

    module B where
        {-# DEPRECATED T, f "Don't use" #-}
        data T = T
        f T = T

One might conceivably want to report deprecation warnings when compiling
ASig with -sig-of B, in which case we need to look at B.hi to find the
deprecation warnings during renaming.  At the moment, you don't get any
warning until you use the identifier further downstream.  This would
require adjusting addUsedGRE so that during signature compilation,
we do not report deprecation warnings for LocalDef.  See also
Note [Handling of deprecations]


Note [Type and class operator definitions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to reject all of these unless we have -XTypeOperators (#3265)
   data a :*: b  = ...
   class a :*: b where ...
   data (:*:) a b  = ....
   class (:*:) a b where ...
The latter two mean that we are not just looking for a
*syntactically-infix* declaration, but one that uses an operator
OccName.  We use OccName.isSymOcc to detect that case, which isn't
terribly efficient, but there seems to be no better way.
Can be made to not be exposed
Only used unwrapped in rnAnnProvenance


Note [DisambiguateRecordFields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When we are looking up record fields in record construction or pattern
matching, we can take advantage of the data constructor name to
resolve fields that would otherwise be ambiguous (provided the
-XDisambiguateRecordFields flag is on).

For example, consider:

   data S = MkS { x :: Int }
   data T = MkT { x :: Int }

   e = MkS { x = 3 }

When we are renaming the occurrence of `x` in `e`, instead of looking
`x` up directly (and finding both fields), lookupRecFieldOcc will
search the fields of `MkS` to find the only possible `x` the user can
mean.

Of course, we still have to check the field is in scope, using
lookupGRE_FieldLabel.  The handling of qualified imports is slightly
subtle: the occurrence may be unqualified even if the field is
imported only qualified (but if the occurrence is qualified, the
qualifier must be correct). For example:

   module A where
     data S = MkS { x :: Int }
     data T = MkT { x :: Int }

   module B where
     import qualified A (S(..))
     import A (T(MkT))

     e1 = MkT   { x = 3 }   -- x not in scope, so fail
     e2 = A.MkS { B.x = 3 } -- module qualifier is wrong, so fail
     e3 = A.MkS { x = 3 }   -- x in scope (lack of module qualifier permitted)

In case `e1`, lookupGRE_FieldLabel will return Nothing.  In case `e2`,
lookupGRE_FieldLabel will return the GRE for `A.x`, but then the guard
will fail because the field RdrName `B.x` is qualified and pickGREs
rejects the GRE.  In case `e3`, lookupGRE_FieldLabel will return the
GRE for `A.x` and the guard will succeed because the field RdrName `x`
is unqualified.




Note [Fall back on lookupGlobalOccRn in lookupRecFieldOcc]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Whenever we fail to find the field or it is not in scope, mb_field
will be False, and we fall back on looking it up normally using
lookupGlobalOccRn.  We don't report an error immediately because the
actual problem might be located elsewhere.  For example (#9975):

   data Test = Test { x :: Int }
   pattern Test wat = Test { x = wat }

Here there are multiple declarations of Test (as a data constructor
and as a pattern synonym), which will be reported as an error.  We
shouldn't also report an error about the occurrence of `x` in the
pattern synonym RHS.  However, if the pattern synonym gets added to
the environment first, we will try and fail to find `x` amongst the
(nonexistent) fields of the pattern synonym.

Alternatively, the scope check can fail due to Template Haskell.
Consider (#12130):

   module Foo where
     import M
     b = $(funny)

   module M(funny) where
     data T = MkT { x :: Int }
     funny :: Q Exp
     funny = [| MkT { x = 3 } |]

When we splice, `MkT` is not lexically in scope, so
lookupGRE_FieldLabel will fail.  But there is no need for
disambiguation anyway, because `x` is an original name, and
lookupGlobalOccRn will find it.


Note [Family instance binders]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
  data family F a
  data instance F T = X1 | X2

The 'data instance' decl has an *occurrence* of F (and T), and *binds*
X1 and X2.  (This is unlike a normal data type declaration which would
bind F too.)  So we want an AvailTC F [X1,X2].

Now consider a similar pair:
  class C a where
    data G a
  instance C S where
    data G S = Y1 | Y2

The 'data G S' *binds* Y1 and Y2, and has an *occurrence* of G.

But there is a small complication: in an instance decl, we don't use
qualified names on the LHS; instead we use the class to disambiguate.
Thus:
  module M where
    import Blib( G )
    class C a where
      data G a
    instance C S where
      data G S = Y1 | Y2
Even though there are two G's in scope (M.G and Blib.G), the occurrence
of 'G' in the 'instance C S' decl is unambiguous, because C has only
one associated type called G. This is exactly what happens for methods,
and it is only consistent to do the same thing for types. That's the
role of the function lookupTcdName; the (Maybe Name) give the class of
the encloseing instance decl, if any.



Note [Looking up Exact RdrNames]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Exact RdrNames are generated by Template Haskell.  See Note [Binders
in Template Haskell] in Convert.

For data types and classes have Exact system Names in the binding
positions for constructors, TyCons etc.  For example
    [d| data T = MkT Int |]
when we splice in and Convert to HsSyn RdrName, we'll get
    data (Exact (system Name "T")) = (Exact (system Name "MkT")) ...
These System names are generated by Convert.thRdrName

But, constructors and the like need External Names, not System Names!
So we do the following

 * In RnEnv.newTopSrcBinder we spot Exact RdrNames that wrap a
   non-External Name, and make an External name for it. This is
   the name that goes in the GlobalRdrEnv

 * When looking up an occurrence of an Exact name, done in
   RnEnv.lookupExactOcc, we find the Name with the right unique in the
   GlobalRdrEnv, and use the one from the envt -- it will be an
   External Name in the case of the data type/constructor above.

 * Exact names are also use for purely local binders generated
   by TH, such as    \x_33. x_33
   Both binder and occurrence are Exact RdrNames.  The occurrence
   gets looked up in the LocalRdrEnv by RnEnv.lookupOccRn, and
   misses, because lookupLocalRdrEnv always returns Nothing for
   an Exact Name.  Now we fall through to lookupExactOcc, which
   will find the Name is not in the GlobalRdrEnv, so we just use
   the Exact supplied Name.



Note [Splicing Exact names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider the splice $(do { x <- newName "x"; return (VarE x) })
This will generate a (HsExpr RdrName) term that mentions the
Exact RdrName "x_56" (or whatever), but does not bind it.  So
when looking such Exact names we want to check that it's in scope,
otherwise the type checker will get confused.  To do this we need to
keep track of all the Names in scope, and the LocalRdrEnv does just that;
we consult it with RdrName.inLocalRdrEnvScope.

There is another wrinkle.  With TH and -XDataKinds, consider
   $( [d| data Nat = Zero
          data T = MkT (Proxy 'Zero)  |] )
After splicing, but before renaming we get this:
   data Nat_77{tc} = Zero_78{d}
   data T_79{tc} = MkT_80{d} (Proxy 'Zero_78{tc})  |] )
The occurrence of 'Zero in the data type for T has the right unique,
but it has a TcClsName name-space in its OccName.  (This is set by
the ctxt_ns argument of Convert.thRdrName.)  When we check that is
in scope in the GlobalRdrEnv, we need to look up the DataName namespace
too.  (An alternative would be to make the GlobalRdrEnv also have
a Name -> GRE mapping.)



Note [Template Haskell ambiguity]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The GlobalRdrEnv invariant says that if
  occ -> [gre1, ..., gren]
then the gres have distinct Names (INVARIANT 1 of GlobalRdrEnv).
This is guaranteed by extendGlobalRdrEnvRn (the dups check in add_gre).

So how can we get multiple gres in lookupExactOcc_maybe?  Because in
TH we might use the same TH NameU in two different name spaces.
eg (#7241):
   $(newName "Foo" >>= \o -> return [DataD [] o [] [RecC o []] [''Show]])
Here we generate a type constructor and data constructor with the same
unique, but different name spaces.

It'd be nicer to rule this out in extendGlobalRdrEnvRn, but that would
mean looking up the OccName in every name-space, just in case, and that
seems a bit brutal.  So it's just done here on lookup.  But we might
need to revisit that choice.



Note [Usage for sub-bndrs]
~~~~~~~~~~~~~~~~~~~~~~~~~~
If you have this
   import qualified M( C( f ) )
   instance M.C T where
     f x = x
then is the qualified import M.f used?  Obviously yes.
But the RdrName used in the instance decl is unqualified.  In effect,
we fill in the qualification by looking for f's whose class is M.C
But when adding to the UsedRdrNames we must make that qualification
explicit (saying "used  M.f"), otherwise we get "Redundant import of M.f".

So we make up a suitable (fake) RdrName.  But be careful
   import qualified M
   import M( C(f) )
   instance C T where
     f x = x
Here we want to record a use of 'f', not of 'M.f', otherwise
we'll miss the fact that the qualified import is redundant.

--------------------------------------------------
--              Occurrences
--------------------------------------------------


Note [Promoted variables in types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this (#12686):
   x = True
   data Bad = Bad 'x

The parser treats the quote in 'x as saying "use the term
namespace", so we'll get (Bad x{v}), with 'x' in the
VarName namespace.  If we don't test for this, the renamer
will happily rename it to the x bound at top level, and then
the typecheck falls over because it doesn't have 'x' in scope
when kind-checking.



Note [Demotion]
~~~~~~~~~~~~~~~
When the user writes:
  data Nat = Zero | Succ Nat
  foo :: f Zero -> Int

'Zero' in the type signature of 'foo' is parsed as:
  HsTyVar ("Zero", TcClsName)

When the renamer hits this occurrence of 'Zero' it's going to realise
that it's not in scope. But because it is renaming a type, it knows
that 'Zero' might be a promoted data constructor, so it will demote
its namespace to DataName and do a second lookup.

The final result (after the renamer) will be:
  HsTyVar ("Zero", DataName)


Note [Handling of deprecations]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
* We report deprecations at each *occurrence* of the deprecated thing
  (see #5867)

* We do not report deprecations for locally-defined names. For a
  start, we may be exporting a deprecated thing. Also we may use a
  deprecated thing in the defn of another deprecated things.  We may
  even use a deprecated thing in the defn of a non-deprecated thing,
  when changing a module's interface.

* addUsedGREs: we do not report deprecations for sub-binders:
     - the ".." completion for records
     - the ".." in an export item 'T(..)'
     - the things exported by a module export 'module M'


Note [Used names with interface not loaded]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It's (just) possible to find a used
Name whose interface hasn't been loaded:

a) It might be a WiredInName; in that case we may not load
   its interface (although we could).

b) It might be GHC.Real.fromRational, or GHC.Num.fromInteger
   These are seen as "used" by the renamer (if -XRebindableSyntax)
   is on), but the typechecker may discard their uses
   if in fact the in-scope fromRational is GHC.Read.fromRational,
   (see tcPat.tcOverloadedLit), and the typechecker sees that the type
   is fixed, say, to GHC.Base.Float (see Inst.lookupSimpleInst).
   In that obscure case it won't force the interface in.

In both cases we simply don't permit deprecations;
this is, after all, wired-in stuff.




Note [Safe Haskell and GHCi]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We DON'T do this Safe Haskell as we need to check imports. We can
and should instead check the qualified import but at the moment
this requires some refactoring so leave as a TODO


Note [Looking up signature names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
lookupSigOccRn is used for type signatures and pragmas
Is this valid?
  module A
        import M( f )
        f :: Int -> Int
        f x = x
It's clear that the 'f' in the signature must refer to A.f
The Haskell98 report does not stipulate this, but it will!
So we must treat the 'f' in the signature in the same way
as the binding occurrence of 'f', using lookupBndrRn

However, consider this case:
        import M( f )
        f :: Int -> Int
        g x = x
We don't want to say 'f' is out of scope; instead, we want to
return the imported 'f', so that later on the reanamer will
correctly report "misplaced type sig".



Note [Signatures for top level things]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data HsSigCtxt = ... | TopSigCtxt NameSet | ....

* The NameSet says what is bound in this group of bindings.
  We can't use isLocalGRE from the GlobalRdrEnv, because of this:
       f x = x
       $( ...some TH splice... )
       f :: Int -> Int
  When we encounter the signature for 'f', the binding for 'f'
  will be in the GlobalRdrEnv, and will be a LocalDef. Yet the
  signature is mis-placed

* For type signatures the NameSet should be the names bound by the
  value bindings; for fixity declarations, the NameSet should also
  include class sigs and record selectors

      infix 3 `f`          -- Yes, ok
      f :: C a => a -> a   -- No, not ok
      class C a where
        f :: a -> a


Note [dataTcOccs and Exact Names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Exact RdrNames can occur in code generated by Template Haskell, and generally
those references are, well, exact. However, the TH `Name` type isn't expressive
enough to always track the correct namespace information, so we sometimes get
the right Unique but wrong namespace. Thus, we still have to do the double-lookup
for Exact RdrNames.

There is also an awkward situation for built-in syntax. Example in GHCi
   :info []
This parses as the Exact RdrName for nilDataCon, but we also want
the list type constructor.

Note that setRdrNameSpace on an Exact name requires the Name to be External,
which it always is for built in syntax.

