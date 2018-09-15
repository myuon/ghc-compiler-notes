[[src]](https://github.com/ghc/ghc/tree/master/compiler/basicTypes/OccName.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


# \subsection{Name space}


# \subsection[Name-pieces-datatypes]{The @OccName@ datatypes}


# \subsection{Printing}


### Note: Suppressing uniques in OccNames

### Note: Unique OccNames from Template Haskell

# \subsection{Construction}


 | Other names in the compiler add additional information to an OccName.
This class provides a consistent way to access the underlying OccName. 

# Environments


OccEnvs are used mainly for the envts in ModIfaces.

### Note: The Unique of an OccName

They are efficient, because FastStrings have unique Int# keys.  We assume
this key is less than 2^24, and indeed FastStrings are allocated keys
sequentially starting at 0.

So we can make a Unique using
        mkUnique ns key  :: Unique
where 'ns' is a Char representing the name space.  This in turn makes it
easy to build an OccEnv.


# \subsection{Predicates and taking them apart}


# \subsection{Making system names}


Here's our convention for splitting up the interface file name space:

   d...         dictionary identifiers
                (local variables, so no name-clash worries)

All of these other OccNames contain a mixture of alphabetic
and symbolic characters, and hence cannot possibly clash with
a user-written type or function name

   $f...        Dict-fun identifiers (from inst decls)
   $dmop        Default method for 'op'
   $pnC         n'th superclass selector for class C
   $wf          Worker for function 'f'
   $sf..        Specialised version of f
   D:C          Data constructor for dictionary for class C
   NTCo:T       Coercion connecting newtype T with its representation type
   TFCo:R       Coercion connecting a data family to its representation type R

In encoded form these appear as Zdfxxx etc

        :...            keywords (export:, letrec: etc.)
--- I THINK THIS IS WRONG!

This knowledge is encoded in the following functions.

@mk_deriv@ generates an @OccName@ from the prefix and a string.
NB: The string must already be encoded!



Sometimes we need to pick an OccName that has not already been used,
given a set of in-use OccNames.



We used to add a '$m' to indicate a method, but that gives rise to bad
error messages from the type checker when we print the function name or pattern
of an instance-decl binding.  Why? Because the binding is zapped
to use the method name in place of the selector name.
(See TcClassDcl.tcMethodBind)

The way it is now, -ddump-xx output may look confusing, but
you can always say -dppr-debug to get the uniques.

However, we *do* have to zap the first character to be lower case,
because overloaded constructors (blarg) generate methods too.
And convert to VarName space

e.g. a call to constructor MkFoo where
        data (Ord a) => Foo a = MkFoo a

If this is necessary, we do it by prefixing '$m'.  These
guys never show up in error messages.  What a hack.


# \subsection{Tidying them up}


Before we print chunks of code we like to rename it so that
we don't have to print lots of silly uniques in it.  But we mustn't
accidentally introduce name clashes!  So the idea is that we leave the
OccName alone unless it accidentally clashes with one that is already
in scope; if so, we tack on '1' at the end and try again, then '2', and
so on till we find a unique one.

There's a wrinkle for operators.  Consider '>>='.  We can't use '>>=1'
because that isn't a single lexeme.  So we encode it to 'lle' and *then*
tack on the '1', if necessary.

### Note: TidyOccEnv

type TidyOccEnv = UniqFM Int

* Domain = The OccName's FastString. These FastStrings are "taken";
           make sure that we don't re-use

* Int, n = A plausible starting point for new guesses
           There is no guarantee that "FSn" is available;
           you must look that up in the TidyOccEnv.  But
           it's a good place to start looking.

* When looking for a renaming for "foo2" we strip off the "2" and start
  with "foo".  Otherwise if we tidy twice we get silly names like foo23.

  However, if it started with digits at the end, we always make a name
  with digits at the end, rather than shortening "foo2" to just "foo",
  even if "foo" is unused.  Reasons:
     - Plain "foo" might be used later
     - We use trailing digits to subtly indicate a unification variable
       in typechecker error message; see TypeRep.tidyTyVarBndr

We have to take care though! Consider a machine-generated module (Trac #10370)
  module Foo where
     a1 = e1
     a2 = e2
     ...
     a2000 = e2000
Then "a1", "a2" etc are all marked taken.  But now if we come across "a7" again,
we have to do a linear search to find a free one, "a2001".  That might just be
acceptable once.  But if we now come across "a8" again, we don't want to repeat
that search.

So we use the TidyOccEnv mapping for "a" (not "a7" or "a8") as our base for
starting the search; and we make sure to update the starting point for "a"
after we allocate a new one.

# Node [Tidying multiple names at once]


Consider

    > :t (id,id,id)

Every id contributes a type variable to the type signature, and all of them are
"a". If we tidy them one by one, we get

    (id,id,id) :: (a2 -> a2, a1 -> a1, a -> a)

which is a bit unfortunate, as it unfairly renames only one of them. What we
would like to see is

    (id,id,id) :: (a3 -> a3, a2 -> a2, a1 -> a1)

To achieve this, the function avoidClashesOccEnv can be used to prepare the
TidyEnv, by “blocking” every name that occurs twice in the map. This way, none
of the "a"s will get the privilege of keeping this name, and all of them will
get a suitable number by tidyOccName.

This prepared TidyEnv can then be used with tidyOccName. See tidyTyCoVarBndrs
for an example where this is used.

This is #12382.



# Binary instance
    Here rather than BinIface because OccName is abstract
