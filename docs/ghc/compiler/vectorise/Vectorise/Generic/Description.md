[[src]](https://github.com/ghc/ghc/tree/master/compiler/vectorise/Vectorise/Generic/Description.hs)
### Note: PData TyCons

When PData is a type family, the compiler generates a type constructor for each
instance, which is named after the family and instance type. This type
constructor does not appear in the source program. Rather, it is implicitly
defined by the data instance. For example with:

  data family PData a

  data instance PData (Sum2 a b)
        = PSum2  U.Sel2
                 (PData a)
                 (PData b)

The type constructor corresponding to the instance will be named 'PDataSum2',
and this is what we will get in the repr_psum_tc field of SumRepr.Sum.

