`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/template-haskell/Language/Haskell/TH/Syntax.hs>`_

libraries/template-haskell/Language/Haskell/TH/Syntax.hs
========================================================


Note [Role of TExp]
~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/template-haskell/Language/Haskell/TH/Syntax.hs#L255>`__

TExp's argument must have a nominal role, not phantom as would
be inferred (#8459).  Consider

::

  e :: TExp Age
  e = MkAge 3

..

::

  foo = $(coerce e) + 4::Int

..

The splice will evaluate to (MkAge 3) and you can't add that to
4::Int. So you can't coerce a (TExp Age) to a (TExp Int). 



Note [Name lookup]
~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/template-haskell/Language/Haskell/TH/Syntax.hs#L342>`__

The functions 'lookupTypeName' and 'lookupValueName' provide
a way to query the current splice's context for what names
are in scope. The function 'lookupTypeName' queries the type
namespace, whereas 'lookupValueName' queries the value namespace,
but the functions are otherwise identical.

A call @lookupValueName s@ will check if there is a value
with name @s@ in scope at the current splice's location. If
there is, the @Name@ of this value is returned;
if not, then @Nothing@ is returned.

The returned name cannot be \"captured\".
For example:

> f = "global"
> g = $( do
>          Just nm <- lookupValueName "f"
>          [| let f = "local" in $( varE nm ) |]

In this case, @g = \"global\"@; the call to @lookupValueName@
returned the global @f@, and this name was /not/ captured by
the local definition of @f@.

The lookup is performed in the context of the /top-level/ splice
being run. For example:

> f = "global"
> g = $( [| let f = "local" in
>            $(do
>                Just nm <- lookupValueName "f"
>                varE nm
>             ) |] )

Again in this example, @g = \"global\"@, because the call to
@lookupValueName@ queries the context of the outer-most @$(...)@.

Operators should be queried without any surrounding parentheses, like so:

> lookupValueName "+"

Qualified names are also supported, like so:

> lookupValueName "Prelude.+"
> lookupValueName "Prelude.map"



Note [Data for non-algebraic types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/template-haskell/Language/Haskell/TH/Syntax.hs#L897>`__

Class Data was originally intended for algebraic data types.  But
it is possible to use it for abstract types too.  For example, in
package `text` we find

  instance Data Text where
    ...
    toConstr _ = packConstr

::

  packConstr :: Constr
  packConstr = mkConstr textDataType "pack" [] Prefix

..

Here `packConstr` isn't a real data constructor, it's an ordinary
function.  Two complications

* In such a case, we must take care to build the Name using
  mkNameG_v (for values), not mkNameG_d (for data constructors).
  See #10796.

* The pseudo-constructor is named only by its string, here "pack".
  But 'dataToQa' needs the TyCon of its defining module, and has
  to assume it's defined in the same module as the TyCon itself.
  But nothing enforces that; #12596 shows what goes wrong if
  "pack" is defined in a different module than the data type "Text".



Note [Unresolved infix]
~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/template-haskell/Language/Haskell/TH/Syntax.hs#L1543>`__

When implementing antiquotation for quasiquoters, one often wants
to parse strings into expressions:

> parse :: String -> Maybe Exp

But how should we parse @a + b * c@? If we don't know the fixities of
@+@ and @*@, we don't know whether to parse it as @a + (b * c)@ or @(a
+ b) * c@.

In cases like this, use 'UInfixE', 'UInfixP', or 'UInfixT', which stand for
\"unresolved infix expression/pattern/type\", respectively. When the compiler
is given a splice containing a tree of @UInfixE@ applications such as

> UInfixE
>   (UInfixE e1 op1 e2)
>   op2
>   (UInfixE e3 op3 e4)

it will look up and the fixities of the relevant operators and
reassociate the tree as necessary.

  * trees will not be reassociated across 'ParensE', 'ParensP', or 'ParensT',
    which are of use for parsing expressions like

::

    > (a + b * c) + d * e

..

  * 'InfixE', 'InfixP', and 'InfixT' expressions are never reassociated.

  * The 'UInfixE' constructor doesn't support sections. Sections
    such as @(a *)@ have no ambiguity, so 'InfixE' suffices. For longer
    sections such as @(a + b * c -)@, use an 'InfixE' constructor for the
    outer-most section, and use 'UInfixE' constructors for all
    other operators:

::

    > InfixE
    >   Just (UInfixE ...a + b * c...)
    >   op
    >   Nothing

..

::

    Sections such as @(a + b +)@ and @((a + b) +)@ should be rendered
    into 'Exp's differently:

..

::

    > (+ a + b)   ---> InfixE Nothing + (Just $ UInfixE a + b)
    >                    -- will result in a fixity error if (+) is left-infix
    > (+ (a + b)) ---> InfixE Nothing + (Just $ ParensE $ UInfixE a + b)
    >                    -- no fixity errors

..

  * Quoted expressions such as

::

    > [| a * b + c |] :: Q Exp
    > [p| a : b : c |] :: Q Pat
    > [t| T + T |] :: Q Type

..

::

    will never contain 'UInfixE', 'UInfixP', 'UInfixT', 'InfixT', 'ParensE',
    'ParensP', or 'ParensT' constructors.

..



Note [GADT return type]
~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/template-haskell/Language/Haskell/TH/Syntax.hs#L2061>`__

The return type of a GADT constructor does not necessarily match the name of
the data type:

type S = T

data T a where
    MkT :: S Int


type S a = T

data T a where
    MkT :: S Char Int


type Id a = a
type S a = T

data T a where
    MkT :: Id (S Char Int)


That is why we allow the return type stored by a constructor to be an
arbitrary type. See also #11341



Note [Representing concrete syntax in types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/template-haskell/Language/Haskell/TH/Syntax.hs#L2192>`__

Haskell has a rich concrete syntax for types, including
  t1 -> t2, (t1,t2), [t], and so on
In TH we represent all of this using AppT, with a distinguished
type constructor at the head.  So,
  Type              TH representation
  -----------------------------------------------
  t1 -> t2          ArrowT `AppT` t2 `AppT` t2
  [t]               ListT `AppT` t
  (t1,t2)           TupleT 2 `AppT` t1 `AppT` t2
  '(t1,t2)          PromotedTupleT 2 `AppT` t1 `AppT` t2

But if the original HsSyn used prefix application, we won't use
these special TH constructors.  For example
  [] t              ConT "[]" `AppT` t
  (->) t            ConT "->" `AppT` t
In this way we can faithfully represent in TH whether the original
HsType used concrete syntax or not.

The one case that doesn't fit this pattern is that of promoted lists
  '[ Maybe, IO ]    PromotedListT 2 `AppT` t1 `AppT` t2
but it's very smelly because there really is no type constructor
corresponding to PromotedListT. So we encode HsExplicitListTy with
PromotedConsT and PromotedNilT (which *do* have underlying type
constructors):
  '[ Maybe, IO ]    PromotedConsT `AppT` Maybe `AppT`
                    (PromotedConsT  `AppT` IO `AppT` PromotedNilT)

