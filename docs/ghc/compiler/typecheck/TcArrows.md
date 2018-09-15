[[src]](https://github.com/ghc/ghc/tree/master/compiler/typecheck/TcArrows.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998

Typecheck arrow notation


### Note: Arrow overview

Here's a summary of arrows and how they typecheck.  First, here's
a cut-down syntax:

  expr ::= ....
        |  proc pat cmd

  cmd ::= cmd exp                    -- Arrow application
       |  \pat -> cmd                -- Arrow abstraction
       |  (| exp cmd1 ... cmdn |)    -- Arrow form, n>=0
       |  ... -- If, case in the usual way

  cmd_type ::= carg_type --> type

  carg_type ::= ()
             |  (type, carg_type)

Note that
 * The 'exp' in an arrow form can mention only
   "arrow-local" variables

 * An "arrow-local" variable is bound by an enclosing
   cmd binding form (eg arrow abstraction)

 * A cmd_type is here written with a funny arrow "-->",
   The bit on the left is a carg_type (command argument type)
   which itself is a nested tuple, finishing with ()

 * The arrow-tail operator (e1 -< e2) means
       (| e1 <<< arr snd |) e2

# Proc


# Commands


# Stmts


# Helpers


# Errors
