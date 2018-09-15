[[src]](https://github.com/ghc/ghc/tree/master/compiler/parser/RdrHsSyn.hs)
# 

  Construction functions for Rdr stuff

# 

# 

  #cvBinds-etc# Converting to @HsBinds@, etc.

# 

# 

  #PrefixToHS-utils# Utilities for conversion

# 

### Note: Parsing data constructors is hard

We parse the RHS of the constructor declaration
     data T = C t1 t2
as a btype_no_ops (treating C as a type constructor) and then convert C to be
a data constructor.  Reason: it might continue like this:
     data T = C t1 t2 :% D Int
in which case C really /would/ be a type constructor.  We can't resolve this
ambiguity till we come across the constructor oprerator :% (or not, more usually)

So the plan is:

* Parse the data constructor declration as a type (actually btype_no_ops)

* Use 'splitCon' to rejig it into the data constructor and the args

* In doing so, we use 'tyConToDataCon' to convert the RdrName for
  the data con, which has been parsed as a tycon, back to a datacon.
  This is more than just adjusting the name space; for operators we
  need to check that it begins with a colon.  E.g.
     data T = (+++)
  will parse ok (since tycons can be operators), but we should reject
  it (Trac #12051).


### Note: setRdrNameSpace for wired-in names

In GHC.Types, which declares (:), we have
  infixr 5 :
The ambiguity about which ":" is meant is resolved by parsing it as a
data constructor, but then using dataTcOccs to try the type constructor too;
and that in turn calls setRdrNameSpace to change the name-space of ":" to
tcClsName.  There isn't a corresponding ":" type constructor, but it's painful
to make setRdrNameSpace partial, so we just make an Unqual name instead. It
really doesn't matter!


 cmdStmts 