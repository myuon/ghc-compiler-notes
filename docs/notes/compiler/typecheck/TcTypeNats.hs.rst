Note [Type-level literals]
~~~~~~~~~~~~~~~~~~~~~~~~~~
There are currently two forms of type-level literals: natural numbers, and
symbols (even though this module is named TcTypeNats, it covers both).

Type-level literals are supported by CoAxiomRules (conditional axioms), which
power the built-in type families (see Note [Adding built-in type families]).
Currently, all built-in type families are for the express purpose of supporting
type-level literals.

See also the Wiki page:

    https://ghc.haskell.org/trac/ghc/wiki/TypeNats



Note [Adding built-in type families]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are a few steps to adding a built-in type family:

* Adding a unique for the type family TyCon

  These go in PrelNames. It will likely be of the form
  @myTyFamNameKey = mkPreludeTyConUnique xyz@, where @xyz@ is a number that
  has not been chosen before in PrelNames. There are several examples already
  in PrelNames—see, for instance, typeNatAddTyFamNameKey.

* Adding the type family TyCon itself

  This goes in TcTypeNats. There are plenty of examples of how to define
  these—see, for instance, typeNatAddTyCon.

  Once your TyCon has been defined, be sure to:

  - Export it from TcTypeNats. (Not doing so caused #14632.)
  - Include it in the typeNatTyCons list, defined in TcTypeNats.

* Exposing associated type family axioms

  When defining the type family TyCon, you will need to define an axiom for
  the type family in general (see, for instance, axAddDef), and perhaps other
  auxiliary axioms for special cases of the type family (see, for instance,
  axAdd0L and axAdd0R).

  After you have defined all of these axioms, be sure to include them in the
  typeNatCoAxiomRules list, defined in TcTypeNats.
  (Not doing so caused #14934.)

* Define the type family somewhere

  Finally, you will need to define the type family somewhere, likely in @base@.
  Currently, all of the built-in type families are defined in GHC.TypeLits or
  GHC.TypeNats, so those are likely candidates.

  Since the behavior of your built-in type family is specified in TcTypeNats,
  you should give an open type family definition with no instances, like so:

    type family MyTypeFam (m :: Nat) (n :: Nat) :: Nat

  Changing the argument and result kinds as appropriate.

* Update the relevant test cases

  The GHC test suite will likely need to be updated after you add your built-in
  type family. For instance:

  - The T9181 test prints the :browse contents of GHC.TypeLits, so if you added
    a test there, the expected output of T9181 will need to change.
  - The TcTypeNatSimple and TcTypeSymbolSimple tests have compile-time unit
    tests, as well as TcTypeNatSimpleRun and TcTypeSymbolSimpleRun, which have
    runtime unit tests. Consider adding further unit tests to those if your
    built-in type family deals with Nats or Symbols, respectively.
------------------------------------------------------------------------------
Built-in type constructors for functions on type-level nats
The list of built-in type family TyCons that GHC uses.
If you define a built-in type family, make sure to add it to this list.
See Note [Adding built-in type families]


Note [Weakened interaction rule for subtraction]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A simpler interaction here might be:

  `s - t ~ r` --> `t + r ~ s`

This would enable us to reuse all the code for addition.
Unfortunately, this works a little too well at the moment.
Consider the following example:

    0 - 5 ~ r --> 5 + r ~ 0 --> (5 = 0, r = 0)

This (correctly) spots that the constraint cannot be solved.

However, this may be a problem if the constraint did not
need to be solved in the first place!  Consider the following example:

f :: Proxy (If (5 <=? 0) (0 - 5) (5 - 0)) -> Proxy 5
f = id

Currently, GHC is strict while evaluating functions, so this does not
work, because even though the `If` should evaluate to `5 - 0`, we
also evaluate the "then" branch which generates the constraint `0 - 5 ~ r`,
which fails.

So, for the time being, we only add an improvement when the RHS is a constant,
which happens to work OK for the moment, although clearly we need to do
something more general.
