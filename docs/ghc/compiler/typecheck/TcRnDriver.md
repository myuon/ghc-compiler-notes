[[src]](https://github.com/ghc/ghc/tree/master/compiler/typecheck/TcRnDriver.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

# Typechecking a whole module

https://ghc.haskell.org/trac/ghc/wiki/Commentary/Compiler/TypeChecker


# Typecheck and rename a module


# Import declarations


# Type-checking the top level of a module


# Compiling hs-boot source files, and
        comparing the hi-boot interface with the real thing



Once we've typechecked the body of the module, we want to compare what
we've found (gathered in a TypeEnv) with the hi-boot details (if any).



        -- Hypothetically, if we were allow to non-nullary type synonyms, here
        -- is how you would check the roles
        if length tvs == length roles1
            then checkRoles roles1 roles2
            else case tcSplitTyConApp_maybe ty of
                    Just (tc2', args) ->
                        checkRoles roles1 (drop (length args) (tyConRoles tc2') ++ roles2)
                    Nothing -> Just roles_msg


# Type-checking the top level of a module (continued)


# Checking for 'main'


### Note: Root-main Id

The function that the RTS invokes is always :Main.main, which we call
root_main_id.  (Because GHC allows the user to have a module not
called Main as the main module, we can't rely on the main function
being called "Main.main".  That's why root_main_id has a fixed module
":Main".)

This is unusual: it's a LocalId whose Name has a Module from another
module.  Tiresomely, we must filter it out again in MkIface, les we
get two defns for 'main' in the interface file!

# GHCi stuff


### Note: Initialising the type environment for GHCi

Most of the the Ids in ic_things, defined by the user in 'let' stmts,
have closed types. E.g.
   ghci> let foo x y = x && not y

However the GHCi debugger creates top-level bindings for Ids whose
types have free RuntimeUnk skolem variables, standing for unknown
types.  If we don't register these free TyVars as global TyVars then
the typechecker will try to quantify over them and fall over in
zonkQuantifiedTyVar. so we must add any free TyVars to the
typechecker's global TyVar set.  That is most conveniently by using
tcExtendLocalTypeEnv, which automatically extends the global TyVar
set.

We do this by splitting out the Ids with open types, using 'is_closed'
to do the partition.  The top-level things go in the global TypeEnv;
the open, NotTopLevel, Ids, with free RuntimeUnk tyvars, go in the
local TypeEnv.

Note that we don't extend the local RdrEnv (tcl_rdr); all the in-scope
things are already in the interactive context's GlobalRdrEnv.
Extending the local RdrEnv isn't terrible, but it means there is an
entry for the same Name in both global and local RdrEnvs, and that
lead to duplicate "perhaps you meant..." suggestions (e.g. T5564).

We don't bother with the tcl_th_bndrs environment either.


 ---------------------------------------------
   At one stage I removed any shadowed bindings from the type_env;
   they are inaccessible but might, I suppose, cause a space leak if we leave them there.
   However, with Template Haskell they aren't necessarily inaccessible.  Consider this
   GHCi session
         Prelude> let f n = n * 2 :: Int
         Prelude> fName <- runQ [| f |]
         Prelude> $(return $ AppE fName (LitE (IntegerL 7)))
         14
         Prelude> let f n = n * 3 :: Int
         Prelude> $(return $ AppE fName (LitE (IntegerL 7)))
   In the last line we use 'fName', which resolves to the *first* 'f'
   in scope. If we delete it from the type env, GHCi crashes because
   it doesn't expect that.

   Hence this code is commented out

-------------------------------------------------- 


--------------------------------------------------------------------------
                Typechecking Stmts in GHCi

Here is the grand plan, implemented in tcUserStmt

        What you type                   The IO [HValue] that hscStmt returns
        -------------                   ------------------------------------
        let pat = expr          ==>     let pat = expr in return [coerce HVal x, coerce HVal y, ...]
                                        bindings: [x,y,...]

        pat <- expr             ==>     expr >>= \ pat -> return [coerce HVal x, coerce HVal y, ...]
                                        bindings: [x,y,...]

        expr (of IO type)       ==>     expr >>= \ it -> return [coerce HVal it]
          [NB: result not printed]      bindings: [it]

        expr (of non-IO type,   ==>     let it = expr in print it >> return [coerce HVal it]
          result showable)              bindings: [it]

        expr (of non-IO type,
          result not showable)  ==>     error


 No sig vars 

### Note: TcRnExprMode

How should we infer a type when a user asks for the type of an expression e
at the GHCi prompt? We offer 3 different possibilities, described below. Each
considers this example, with -fprint-explicit-foralls enabled:

  foo :: forall a f b. (Show a, Num b, Foldable f) => a -> f b -> String
  :type{,-spec,-def} foo @Int

:type / TM_Inst

  In this mode, we report the type that would be inferred if a variable
  were assigned to expression e, without applying the monomorphism restriction.
  This means we deeply instantiate the type and then regeneralize, as discussed
  in #11376.

  > :type foo @Int
  forall {b} {f :: * -> *}. (Foldable f, Num b) => Int -> f b -> String

  Note that the variables and constraints are reordered here, because this
  is possible during regeneralization. Also note that the variables are
  reported as Inferred instead of Specified.

:type +v / TM_NoInst

  This mode is for the benefit of users using TypeApplications. It does no
  instantiation whatsoever, sometimes meaning that class constraints are not
  solved.

  > :type +v foo @Int
  forall f b. (Show Int, Num b, Foldable f) => Int -> f b -> String

  Note that Show Int is still reported, because the solver never got a chance
  to see it.

:type +d / TM_Default

  This mode is for the benefit of users who wish to see instantiations of
  generalized types, and in particular to instantiate Foldable and Traversable.
  In this mode, any type variable that can be defaulted is defaulted. Because
  GHCi uses -XExtendedDefaultRules, this means that Foldable and Traversable are
  defaulted.

  > :type +d foo @Int
  Int -> [Integer] -> String

  Note that this mode can sometimes lead to a type error, if a type variable is
  used with a defaultable class but cannot actually be defaulted:

  bar :: (Num a, Monoid a) => a -> a
  > :type +d bar
  ** error **

  The error arises because GHC tries to default a but cannot find a concrete
  type in the defaulting list that is both Num and Monoid. (If this list is
  modified to include an element that is both Num and Monoid, the defaulting
  would succeed, of course.)

### Note: Kind-generalise in tcRnType

We switch on PolyKinds when kind-checking a user type, so that we will
kind-generalise the type, even when PolyKinds is not otherwise on.
This gives the right default behaviour at the GHCi prompt, where if
you say ":k T", and T has a polymorphic kind, you'd like to see that
polymorphism. Of course.  If T isn't kind-polymorphic you won't get
anything unexpected, but the apparent *loss* of polymorphism, for
types that you know are polymorphic, is quite surprising.  See Trac
#7688 for a discussion.

Note that the goal is to generalise the *kind of the type*, not
the type itself! Example:
  ghci> data T m a = MkT (m a)  -- T :: forall . (k -> *) -> k -> *
  ghci> :k T
We instantiate T to get (T kappa).  We do not want to kind-generalise
that to forall k. T k!  Rather we want to take its kind
   T kappa :: (kappa -> *) -> kappa -> *
and now kind-generalise that kind, to forall k. (k->*) -> k -> *
(It was Trac #10122 that made me realise how wrong the previous
approach was.) 

# tcRnDeclsi


tcRnDeclsi exists to allow class, data, and other declarations in GHCi.


# More GHCi stuff, to do with browsing and getting info


# Degugging output


# 

Type Checker Plugins

# 