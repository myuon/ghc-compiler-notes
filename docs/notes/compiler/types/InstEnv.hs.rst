`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/types/InstEnv.hs>`_

Note [ClsInst laziness and the rough-match fields]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we load 'instance A.C B.T' from A.hi, but suppose that the type B.T is
otherwise unused in the program. Then it's stupid to load B.hi, the data type
declaration for B.T -- and perhaps further instance declarations!

We avoid this as follows:

* is_cls_nm, is_tcs, is_dfun_name are all Names. We can poke them to our heart's
  content.

* Proper-match fields. is_dfun, and its related fields is_tvs, is_cls, is_tys
  contain TyVars, Class, Type, Class etc, and so are all lazy thunks. When we
  poke any of these fields we'll typecheck the DFunId declaration, and hence
  pull in interfaces that it refers to. See Note [Proper-match fields].

* Rough-match fields. During instance lookup, we use the is_cls_nm :: Name and
  is_tcs :: [Maybe Name] fields to perform a "rough match", *without* poking
  inside the DFunId. The rough-match fields allow us to say "definitely does not
  match", based only on Names.

  This laziness is very important; see #12367. Try hard to avoid pulling on
  the structured fields unless you really need the instance.

* Another place to watch is InstEnv.instIsVisible, which needs the module to
  which the ClsInst belongs. We can get this from is_dfun_name.

* In is_tcs,
    Nothing  means that this type arg is a type variable

    (Just n) means that this type arg is a
                TyConApp with a type constructor of n.
                This is always a real tycon, never a synonym!
                (Two different synonyms might match, but two
                different real tycons can't.)
                NB: newtypes are not transparent, though!


Note [Template tyvars are fresh]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The is_tvs field of a ClsInst has *completely fresh* tyvars.
That is, they are
  * distinct from any other ClsInst
  * distinct from any tyvars free in predicates that may
    be looked up in the class instance environment
Reason for freshness: we use unification when checking for overlap
etc, and that requires the tyvars to be distinct.

The invariant is checked by the ASSERT in lookupInstEnv'.



Note [Proper-match fields]
~~~~~~~~~~~~~~~~~~~~~~~~~
The is_tvs, is_cls, is_tys fields are simply cached values, pulled
out (lazily) from the dfun id. They are cached here simply so
that we don't need to decompose the DFunId each time we want
to match it.  The hope is that the rough-match fields mean
that we often never poke the proper-match fields.

However, note that:
 * is_tvs must be a superset of the free vars of is_tys

 * is_tvs, is_tys may be alpha-renamed compared to the ones in
   the dfun Id



Note [Haddock assumptions]
~~~~~~~~~~~~~~~~~~~~~~~~~~
For normal user-written instances, Haddock relies on

 * the SrcSpan of
 * the Name of
 * the is_dfun of
 * an Instance

being equal to

  * the SrcSpan of
  * the instance head type of
  * the InstDecl used to construct the Instance.


Note [When exactly is an instance decl an orphan?]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  (see MkIface.instanceToIfaceInst, which implements this)
Roughly speaking, an instance is an orphan if its head (after the =>)
mentions nothing defined in this module.

Functional dependencies complicate the situation though. Consider

  module M where { class C a b | a -> b }

and suppose we are compiling module X:

  module X where
        import M
        data T = ...
        instance C Int T where ...

This instance is an orphan, because when compiling a third module Y we
might get a constraint (C Int v), and we'd want to improve v to T.  So
we must make sure X's instances are loaded, even if we do not directly
use anything from X.

More precisely, an instance is an orphan iff

  If there are no fundeps, then at least of the names in
  the instance head is locally defined.

  If there are fundeps, then for every fundep, at least one of the
  names free in a *non-determined* part of the instance head is
  defined in this module.

(Note that these conditions hold trivially if the class is locally
defined.)




Note [InstEnv determinism]
~~~~~~~~~~~~~~~~~~~~~~~~~~
We turn InstEnvs into a list in some places that don't directly affect
the ABI. That happens when we create output for `:info`.
Unfortunately that nondeterminism is nonlocal and it's hard to tell what it
affects without following a chain of functions. It's also easy to accidentally
make that nondeterminism affect the ABI. Furthermore the envs should be
relatively small, so it should be free to use deterministic maps here.
Testing with nofib and validate detected no difference between UniqFM and
UniqDFM. See also Note [Deterministic UniqFM]


Note [Instance lookup and orphan instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Suppose we are compiling a module M, and we have a zillion packages
loaded, and we are looking up an instance for C (T W).  If we find a
match in module 'X' from package 'p', should be "in scope"; that is,

  is p:X in the transitive closure of modules imported from M?

The difficulty is that the "zillion packages" might include ones loaded
through earlier invocations of the GHC API, or earlier module loads in GHCi.
They might not be in the dependencies of M itself; and if not, the instances
in them should not be visible.  #2182, #8427.

There are two cases:
  * If the instance is *not an orphan*, then module X defines C, T, or W.
    And in order for those types to be involved in typechecking M, it
    must be that X is in the transitive closure of M's imports.  So we
    can use the instance.

  * If the instance *is an orphan*, the above reasoning does not apply.
    So we keep track of the set of orphan modules transitively below M;
    this is the ie_visible field of InstEnvs, of type VisibleOrphanModules.

    If module p:X is in this set, then we can use the instance, otherwise
    we can't.



Note [Rules for instance lookup]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
These functions implement the carefully-written rules in the user
manual section on "overlapping instances". At risk of duplication,
here are the rules.  If the rules change, change this text and the
user manual simultaneously.  The link may be this:
http://www.haskell.org/ghc/docs/latest/html/users_guide/glasgow_exts.html#instance-overlap

The willingness to be overlapped or incoherent is a property of the
instance declaration itself, controlled as follows:

 * An instance is "incoherent"
   if it has an INCOHERENT pragma, or
   if it appears in a module compiled with -XIncoherentInstances.

 * An instance is "overlappable"
   if it has an OVERLAPPABLE or OVERLAPS pragma, or
   if it appears in a module compiled with -XOverlappingInstances, or
   if the instance is incoherent.

 * An instance is "overlapping"
   if it has an OVERLAPPING or OVERLAPS pragma, or
   if it appears in a module compiled with -XOverlappingInstances, or
   if the instance is incoherent.
     compiled with -XOverlappingInstances.

Now suppose that, in some client module, we are searching for an instance
of the target constraint (C ty1 .. tyn). The search works like this.

*  Find all instances `I` that *match* the target constraint; that is, the
   target constraint is a substitution instance of `I`. These instance
   declarations are the *candidates*.

*  Eliminate any candidate `IX` for which both of the following hold:

   -  There is another candidate `IY` that is strictly more specific; that
      is, `IY` is a substitution instance of `IX` but not vice versa.

   -  Either `IX` is *overlappable*, or `IY` is *overlapping*. (This
      "either/or" design, rather than a "both/and" design, allow a
      client to deliberately override an instance from a library,
      without requiring a change to the library.)

-  If exactly one non-incoherent candidate remains, select it. If all
   remaining candidates are incoherent, select an arbitrary one.
   Otherwise the search fails (i.e. when more than one surviving
   candidate is not incoherent).

-  If the selected candidate (from the previous step) is incoherent, the
   search succeeds, returning that candidate.

-  If not, find all instances that *unify* with the target constraint,
   but do not *match* it. Such non-candidate instances might match when
   the target constraint is further instantiated. If all of them are
   incoherent, the search succeeds, returning the selected candidate; if
   not, the search fails.

Notice that these rules are not influenced by flag settings in the
client module, where the instances are *used*. These rules make it
possible for a library author to design a library that relies on
overlapping instances without the client having to know.

Note [Overlapping instances]   (NB: these notes are quite old)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Overlap is permitted, but only in such a way that one can make
a unique choice when looking up.  That is, overlap is only permitted if
one template matches the other, or vice versa.  So this is ok:

  [a]  [Int]

but this is not

  (Int,a)  (b,Int)

If overlap is permitted, the list is kept most specific first, so that
the first lookup is the right choice.


For now we just use association lists.

\subsection{Avoiding a problem with overlapping}

Consider this little program:

\begin{pseudocode}
     class C a        where c :: a
     class C a => D a where d :: a

     instance C Int where c = 17
     instance D Int where d = 13

     instance C a => C [a] where c = [c]
     instance ({- C [a], -} D a) => D [a] where d = c

     instance C [Int] where c = [37]

     main = print (d :: [Int])
\end{pseudocode}

What do you think `main' prints  (assuming we have overlapping instances, and
all that turned on)?  Well, the instance for `D' at type `[a]' is defined to
be `c' at the same type, and we've got an instance of `C' at `[Int]', so the
answer is `[37]', right? (the generic `C [a]' instance shouldn't apply because
the `C [Int]' instance is more specific).

Ghc-4.04 gives `[37]', while ghc-4.06 gives `[17]', so 4.06 is wrong.  That
was easy ;-)  Let's just consult hugs for good measure.  Wait - if I use old
hugs (pre-September99), I get `[17]', and stranger yet, if I use hugs98, it
doesn't even compile!  What's going on!?

What hugs complains about is the `D [a]' instance decl.

\begin{pseudocode}
     ERROR "mj.hs" (line 10): Cannot build superclass instance
     *** Instance            : D [a]
     *** Context supplied    : D a
     *** Required superclass : C [a]
\end{pseudocode}

You might wonder what hugs is complaining about.  It's saying that you
need to add `C [a]' to the context of the `D [a]' instance (as appears
in comments).  But there's that `C [a]' instance decl one line above
that says that I can reduce the need for a `C [a]' instance to the
need for a `C a' instance, and in this case, I already have the
necessary `C a' instance (since we have `D a' explicitly in the
context, and `C' is a superclass of `D').

Unfortunately, the above reasoning indicates a premature commitment to the
generic `C [a]' instance.  I.e., it prematurely rules out the more specific
instance `C [Int]'.  This is the mistake that ghc-4.06 makes.  The fix is to
add the context that hugs suggests (uncomment the `C [a]'), effectively
deferring the decision about which instance to use.

Now, interestingly enough, 4.04 has this same bug, but it's covered up
in this case by a little known `optimization' that was disabled in
4.06.  Ghc-4.04 silently inserts any missing superclass context into
an instance declaration.  In this case, it silently inserts the `C
[a]', and everything happens to work out.

(See `basicTypes/MkId:mkDictFunId' for the code in question.  Search for
`Mark Jones', although Mark claims no credit for the `optimization' in
question, and would rather it stopped being called the `Mark Jones
optimization' ;-)

So, what's the fix?  I think hugs has it right.  Here's why.  Let's try
something else out with ghc-4.04.  Let's add the following line:

    d' :: D a => [a]
    d' = c

Everyone raise their hand who thinks that `d :: [Int]' should give a
different answer from `d' :: [Int]'.  Well, in ghc-4.04, it does.  The
`optimization' only applies to instance decls, not to regular
bindings, giving inconsistent behavior.

Old hugs had this same bug.  Here's how we fixed it: like GHC, the
list of instances for a given class is ordered, so that more specific
instances come before more generic ones.  For example, the instance
list for C might contain:
    ..., C Int, ..., C a, ...
When we go to look for a `C Int' instance we'll get that one first.
But what if we go looking for a `C b' (`b' is unconstrained)?  We'll
pass the `C Int' instance, and keep going.  But if `b' is
unconstrained, then we don't know yet if the more specific instance
will eventually apply.  GHC keeps going, and matches on the generic `C
a'.  The fix is to, at each step, check to see if there's a reverse
match, and if so, abort the search.  This prevents hugs from
prematurely chosing a generic instance when a more specific one
exists.

--Jeff

BUT NOTE [Nov 2001]: we must actually *unify* not reverse-match in
this test.  Suppose the instance envt had
    ..., forall a b. C a a b, ..., forall a b c. C a b c, ...
(still most specific first)
Now suppose we are looking for (C x y Int), where x and y are unconstrained.
        C x y Int  doesn't match the template {a,b} C a a b
but neither does
        C a a b  match the template {x,y} C x y Int
But still x and y might subsequently be unified so they *do* match.

Simple story: unify, don't match.


Note [DFunInstType: instantiating types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
A successful match is a ClsInst, together with the types at which
        the dfun_id in the ClsInst should be instantiated
The instantiating types are (Either TyVar Type)s because the dfun
might have some tyvars that *only* appear in arguments
        dfun :: forall a b. C a b, Ord b => D [a]
When we match this against D [ty], we return the instantiating types
        [Just ty, Nothing]
where the 'Nothing' indicates that 'b' can be freely instantiated.
(The caller instantiates it to a flexi type variable, which will
 presumably later become fixed via functional dependencies.)


Note [Incoherent instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
For some classes, the choice of a particular instance does not matter, any one
is good. E.g. consider

        class D a b where { opD :: a -> b -> String }
        instance D Int b where ...
        instance D a Int where ...

        g (x::Int) = opD x x  -- Wanted: D Int Int

For such classes this should work (without having to add an "instance D Int
Int", and using -XOverlappingInstances, which would then work). This is what
-XIncoherentInstances is for: Telling GHC "I don't care which instance you use;
if you can use one, use it."

Should this logic only work when *all* candidates have the incoherent flag, or
even when all but one have it? The right choice is the latter, which can be
justified by comparing the behaviour with how -XIncoherentInstances worked when
it was only about the unify-check (note [Overlapping instances]):

Example:
        class C a b c where foo :: (a,b,c)
        instance C [a] b Int
        instance [incoherent] [Int] b c
        instance [incoherent] C a Int c
Thanks to the incoherent flags,
        [Wanted]  C [a] b Int
works: Only instance one matches, the others just unify, but are marked
incoherent.

So I can write
        (foo :: ([a],b,Int)) :: ([Int], Int, Int).
but if that works then I really want to be able to write
        foo :: ([Int], Int, Int)
as well. Now all three instances from above match. None is more specific than
another, so none is ruled out by the normal overlapping rules. One of them is
not incoherent, but we still want this to compile. Hence the
"all-but-one-logic".

The implementation is in insert_overlapping, where we remove matching
incoherent instances as long as there are others.





Note [Binding when looking up instances]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When looking up in the instance environment, or family-instance environment,
we are careful about multiple matches, as described above in
Note [Overlapping instances]

The key_tys can contain skolem constants, and we can guarantee that those
are never going to be instantiated to anything, so we should not involve
them in the unification test.  Example:
        class Foo a where { op :: a -> Int }
        instance Foo a => Foo [a]       -- NB overlap
        instance Foo [Int]              -- NB overlap
        data T = forall a. Foo a => MkT a
        f :: T -> Int
        f (MkT x) = op [x,x]
The op [x,x] means we need (Foo [a]).  Without the filterVarSet we'd
complain, saying that the choice of instance depended on the instantiation
of 'a'; but of course it isn't *going* to be instantiated.

We do this only for isOverlappableTyVar skolems.  For example we reject
        g :: forall a => [a] -> Int
        g x = op x
on the grounds that the correct instance depends on the instantiation of 'a'

