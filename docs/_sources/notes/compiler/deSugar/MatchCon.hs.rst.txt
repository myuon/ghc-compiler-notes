Note [Record patterns]
~~~~~~~~~~~~~~~~~~~~~~
Consider
         data T = T { x,y,z :: Bool }

         f (T { y=True, x=False }) = ...

We must match the patterns IN THE ORDER GIVEN, thus for the first
one we match y=True before x=False.  See #246; or imagine
matching against (T { y=False, x=undefined }): should fail without
touching the undefined.

Now consider:

         f (T { y=True, x=False }) = ...
         f (T { x=True, y= False}) = ...

In the first we must test y first; in the second we must test x
first.  So we must divide even the equations for a single constructor
T into sub-goups, based on whether they match the same field in the
same order.  That's what the (groupBy compatible_pats) grouping.

All non-record patterns are "compatible" in this sense, because the
positional patterns (T a b) and (a `T` b) all match the arguments
in order.  Also T {} is special because it's equivalent to (T _ _).
Hence the (null rpats) checks here and there.




Note [Existentials in shift_con_pat]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider
        data T = forall a. Ord a => T a (a->Int)

        f (T x f) True  = ...expr1...
        f (T y g) False = ...expr2..

When we put in the tyvars etc we get

        f (T a (d::Ord a) (x::a) (f::a->Int)) True =  ...expr1...
        f (T b (e::Ord b) (y::a) (g::a->Int)) True =  ...expr2...

After desugaring etc we'll get a single case:

        f = \t::T b::Bool ->
            case t of
               T a (d::Ord a) (x::a) (f::a->Int)) ->
            case b of
                True  -> ...expr1...
                False -> ...expr2...

*** We have to substitute [a/b, d/e] in expr2! **
Hence
                False -> ....((/\b\(e:Ord b).expr2) a d)....

Originally I tried to use
        (\b -> let e = d in expr2) a
to do this substitution.  While this is "correct" in a way, it fails
Lint, because e::Ord b but d::Ord a.

