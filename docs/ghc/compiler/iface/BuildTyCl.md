[[src]](https://github.com/ghc/ghc/tree/master/compiler/iface/BuildTyCl.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


 No fields 

 no existentials 

 No GADT equalities 

 No theta 

### Note: Class newtypes and equality predicates

Consider
        class (a ~ F b) => C a b where
          op :: a -> b

We cannot represent this by a newtype, even though it's not
existential, because there are two value fields (the equality
predicate and op. See Trac #2238

Moreover,
          class (a ~ F b) => C a b where {}
Here we can't use a newtype either, even though there is only
one field, because equality predicates are unboxed, and classes
are boxed.
