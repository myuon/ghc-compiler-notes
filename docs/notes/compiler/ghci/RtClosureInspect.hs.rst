`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/ghci/RtClosureInspect.hs>`_

compiler/ghci/RtClosureInspect.hs
=================================


Note [Constructor arg types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

`[note link] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/ghci/RtClosureInspect.hs#L1057>`__

Consider a GADT (cf #7386)
   data family D a b
   data instance D [a] a where
     MkT :: a -> D [a] (Maybe a)
     ...

In getDataConArgTys
* con_app_ty is the known type (from outside) of the constructor application,
  say D [Int] Int

* The data constructor MkT has a (representation) dataConTyCon = DList,
  say where
    data DList a where
      MkT :: a -> DList a (Maybe a)
      ...

So the dataConTyCon of the data constructor, DList, differs from
the "outside" type, D. So we can't straightforwardly decompose the
"outside" type, and we end up in the "_" branch of the case.

Then we match the dataConOrigResTy of the data constructor against the
outside type, hoping to get a substitution that tells how to instantiate
the *representation* type constructor.   This looks a bit delicate to
me, but it seems to work.
Soundness checks
------------------
This is not formalized anywhere, so hold to your seats!
RTTI in the presence of newtypes can be a tricky and unsound business.

Example:
~~~~~~~~~
Suppose we are doing RTTI for a partially evaluated
closure t, the real type of which is t :: MkT Int, for

::

   newtype MkT a = MkT [Maybe a]

The table below shows the results of RTTI and the improvement
calculated for different combinations of evaluatedness and :type t.
Regard the two first columns as input and the next two as output.

  # |     t     |  :type t  | rtti(t)  | improv.    | result
    ------------------------------------------------------------
  1 |     _     |    t b    |    a     | none       | OK
  2 |     _     |   MkT b   |    a     | none       | OK
  3 |     _     |   t Int   |    a     | none       | OK

::

  If t is not evaluated at *all*, we are safe.

  4 |  (_ : _)  |    t b    |   [a]    | t = []     | UNSOUND
  5 |  (_ : _)  |   MkT b   |  MkT a   | none       | OK (compensating for the missing newtype)
  6 |  (_ : _)  |   t Int   |  [Int]   | t = []     | UNSOUND

::

  If a is a minimal whnf, we run into trouble. Note that
  row 5 above does newtype enrichment on the ty_rtty parameter.

  7 | (Just _:_)|    t b    |[Maybe a] | t = [],    | UNSOUND
    |                       |          | b = Maybe a|

  8 | (Just _:_)|   MkT b   |  MkT a   |  none      | OK
  9 | (Just _:_)|   t Int   |   FAIL   |  none      | OK

::

  And if t is any more evaluated than whnf, we are still in trouble.
  Because constraints are solved in top-down order, when we reach the
  Maybe subterm what we got is already unsound. This explains why the
  row 9 fails to complete.

  10 | (Just _:_)|  t Int  | [Maybe a]   |  FAIL    | OK
  11 | (Just 1:_)|  t Int  | [Maybe Int] |  FAIL    | OK

::

  We can undo the failure in row 9 by leaving out the constraint
  coming from the type signature of t (i.e., the 2nd column).
  Note that this type information is still used
  to calculate the improvement. But we fail
  when trying to calculate the improvement, as there is no unifier for
  t Int = [Maybe a] or t Int = [Maybe Int].


::

  Another set of examples with t :: [MkT (Maybe Int)]  \equiv  [[Maybe (Maybe Int)]]

  # |     t     |    :type t    |  rtti(t)    | improvement | result
    ---------------------------------------------------------------------
  1 |(Just _:_) | [t (Maybe a)] | [[Maybe b]] | t = []      |
    |           |               |             | b = Maybe a |

The checks:
~~~~~~~~~~~
Consider a function obtainType that takes a value and a type and produces
the Term representation and a substitution (the improvement).
Assume an auxiliar rtti' function which does the actual job if recovering
the type, but which may produce a false type.

In pseudocode:

::

  rtti' :: a -> IO Type  -- Does not use the static type information

::

  obtainType :: a -> Type -> IO (Maybe (Term, Improvement))
  obtainType v old_ty = do
       rtti_ty <- rtti' v
       if monomorphic rtti_ty || (check rtti_ty old_ty)
        then ...
         else return Nothing
  where check rtti_ty old_ty = check1 rtti_ty &&
                              check2 rtti_ty old_ty

::

  check1 :: Type -> Bool
  check2 :: Type -> Type -> Bool

Now, if rtti' returns a monomorphic type, we are safe.
If that is not the case, then we consider two conditions.


1. To prevent the class of unsoundness displayed by
   rows 4 and 7 in the example: no higher kind tyvars
   accepted.

::

  check1 (t a)   = NO
  check1 (t Int) = NO
  check1 ([] a)  = YES

2. To prevent the class of unsoundness shown by row 6,
   the rtti type should be structurally more
   defined than the old type we are comparing it to.
  check2 :: NewType -> OldType -> Bool
  check2 a  _        = True
  check2 [a] a       = True
  check2 [a] (t Int) = False
  check2 [a] (t a)   = False  -- By check1 we never reach this equation
  check2 [Int] a     = True
  check2 [Int] (t Int) = True
  check2 [Maybe a]   (t Int) = False
  check2 [Maybe Int] (t Int) = True
  check2 (Maybe [a])   (m [Int]) = False
  check2 (Maybe [Int]) (m [Int]) = True

