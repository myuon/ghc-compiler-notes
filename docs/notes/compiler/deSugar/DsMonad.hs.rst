`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/deSugar/DsMonad.hs>`_

compiler/deSugar/DsMonad.hs
===========================


Note [Levity polymorphism checking]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/deSugar/DsMonad.hs#L307>`__

According to the "Levity Polymorphism" paper (PLDI '17), levity
polymorphism is forbidden in precisely two places: in the type of a bound
term-level argument and in the type of an argument to a function. The paper
explains it more fully, but briefly: expressions in these contexts need to be
stored in registers, and it's hard (read, impossible) to store something
that's levity polymorphic.

We cannot check for bad levity polymorphism conveniently in the type checker,
because we can't tell, a priori, which levity metavariables will be solved.
At one point, I (Richard) thought we could check in the zonker, but it's hard
to know where precisely are the abstracted variables and the arguments. So
we check in the desugarer, the only place where we can see the Core code and
still report respectable syntax to the user. This covers the vast majority
of cases; see calls to DsMonad.dsNoLevPoly and friends.

Levity polymorphism is also prohibited in the types of binders, and the
desugarer checks for this in GHC-generated Ids. (The zonker handles
the user-writted ids in zonkIdBndr.) This is done in newSysLocalDsNoLP.
The newSysLocalDs variant is used in the vast majority of cases where
the binder is obviously not levity polymorphic, omitting the check.
It would be nice to ASSERT that there is no levity polymorphism here,
but we can't, because of the fixM in DsArrows. It's all OK, though:
Core Lint will catch an error here.

However, the desugarer is the wrong place for certain checks. In particular,
the desugarer can't report a sensible error message if an HsWrapper is malformed.
After all, GHC itself produced the HsWrapper. So we store some message text
in the appropriate HsWrappers (e.g. WpFun) that we can print out in the
desugarer.

There are a few more checks in places where Core is generated outside the
desugarer. For example, in datatype and class declarations, where levity
polymorphism is checked for during validity checking. It would be nice to
have one central place for all this, but that doesn't seem possible while
still reporting nice error messages.

Make a new Id with the same print name, but different type, and new unique

