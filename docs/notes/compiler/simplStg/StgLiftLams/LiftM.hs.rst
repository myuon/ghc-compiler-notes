`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplStg/StgLiftLams/LiftM.hs>`_

compiler/simplStg/StgLiftLams/LiftM.hs
======================================


Note [Handling floats]
~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/simplStg/StgLiftLams/LiftM.hs#L94>`__

 Consider the following expression:

 @
     f x =
       let g y = ... f y ...
       in g x
 @

::

 What happens when we want to lift @g@? Normally, we'd put the lifted @l_g@
 binding above the binding for @f@:

..

::

 @
     g f y = ... f y ...
     f x = g f x
 @

..

 But this very unnecessarily turns a known call to @f@ into an unknown one, in
 addition to complicating matters for the analysis.
 Instead, we'd really like to put both functions in the same recursive group,
 thereby preserving the known call:

::

 @
     Rec {
       g y = ... f y ...
       f x = g x
     }
 @

..

 But we don't want this to happen for just /any/ binding. That would create
 possibly huge recursive groups in the process, calling for an occurrence
 analyser on STG.
 So, we need to track when we lift a binding out of a recursive RHS and add
 the binding to the same recursive group as the enclosing recursive binding
 (which must have either already been at the top-level or decided to be
 lifted itself in order to preserve the known call).

::

 This is done by expressing this kind of nesting structure as a 'Writer' over
 @['FloatLang']@ and flattening this expression in 'runLiftM' by a call to
 'collectFloats'.
 API-wise, the analysis will not need to know about the whole 'FloatLang'
 business and will just manipulate it indirectly through actions in 'LiftM'.

..

