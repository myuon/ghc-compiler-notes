[[src]](https://github.com/ghc/ghc/tree/master/compiler/basicTypes/VarEnv.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


# In-scope sets


# Dual renaming


### Note: Eta expansion

When matching
     (\x.M) ~ N
we rename x to x' with, where x' is not in scope in
either term.  Then we want to behave as if we'd seen
     (\x'.M) ~ (\x'.N x')
Since x' isn't in scope in N, the form (\x'. N x') doesn't
capture any variables in N.  But we must nevertheless extend
the envR with a binding [x' -> x'], to support the occurs check.
For example, if we don't do this, we can get silly matches like
        forall a.  (\y.a)  ~   v
succeeding with [a -> v y], which is bogus of course.

# Tidying


# \subsection{@VarEnv@s}



@modifyVarEnv@: Look up a thing in the VarEnv,
then mash it with the modify function, and put it back.
