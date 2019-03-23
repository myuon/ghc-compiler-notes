Note [All alternatives are the binder]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When all alternatives simply refer to the case binder, then we do not have
to bother with the case expression at all (#13588). CoreSTG does this as well,
but sometimes, types get into the way:

    newtype T = MkT Int
    f :: (Int, Int) -> (T, Int)
    f (x, y) = (MkT x, y)

Core cannot just turn this into

    f p = p

as this would not be well-typed. But to STG, where MkT is no longer in the way,
we can.



Note [Trivial case scrutinee]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to be able to handle nested reconstruction of constructors as in

    nested :: Either Int (Either Int a) -> Either Bool (Either Bool a)
    nested (Right (Right v)) = Right (Right v)
    nested _ = Left True

So if we come across

    case x of r1
      Right a -> case a of r2
              Right b -> let v = Right b
                         in Right v

we first replace v with r2. Next we want to replace Right r2 with r1. But the
ce_conAppMap contains Right a!

Therefore, we add r1 ↦ x to ce_bndrMap when analysing the outer case, and use
this substitution before looking Right r2 up in ce_conAppMap, and everything
works out.



Note [Free variables of an StgClosure]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
StgClosures (function and thunks) have an explicit list of free variables:

foo [x] =
    let not_a_free_var = Left [x]
    let a_free_var = Right [x]
    let closure = \[x a_free_var] -> \[y] -> bar y (Left [x]) a_free_var
    in closure

If we were to CSE `Left [x]` in the body of `closure` with `not_a_free_var`,
then the list of free variables would be wrong, so for now, we do not CSE
across such a closure, simply because I (Joachim) was not sure about possible
knock-on effects. If deemed safe and worth the slight code complication of
re-calculating this list during or after this pass, this can surely be done.
