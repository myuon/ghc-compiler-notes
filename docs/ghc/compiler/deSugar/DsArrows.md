[[src]](https://github.com/ghc/ghc/tree/master/compiler/deSugar/DsArrows.hs)

(c) The University of Glasgow 2006
(c) The GRASP/AQUA Project, Glasgow University, 1992-1998


Desugaring arrow commands



Build case analysis of a tuple.  This cannot be done in the DsM monad,
because the list of variables is typically not yet defined.



The input is divided into a local environment, which is a flat tuple
(unless it's too big), and a stack, which is a right-nested pair.
In general, the input has the form

        ((x1,...,xn), (s1,...(sk,())...))

where xi are the environment values, and si the ones on the stack,
with s1 being the "top", the first one to be matched with a lambda.



Translation of a command judgement of the form

        D; xs |-a c : stk --> t

to an expression e such that

        D |- e :: a (xs, stk) t



Case commands are treated in much the same way as if commands
(see above) except that there are more alternatives.  For example

        case e of { p1 -> c1; p2 -> c2; p3 -> c3 }

is translated to

        premap (\ ((xs)*ts) -> case e of
                p1 -> (Left (Left (xs1)*ts))
                p2 -> Left ((Right (xs2)*ts))
                p3 -> Right ((xs3)*ts))
        ((c1 ||| c2) ||| c3)

The idea is to extract the commands from the case, build a balanced tree
of choices, and replace the commands with expressions that build tagged
tuples, obtaining a case expression that can be desugared normally.
To build all this, we use triples describing segments of the list of
case bodies, containing the following fields:
 * a list of expressions of the form (Left|Right)* ((xs)*ts), to be put
   into the case replacing the commands
 * a sum type that is the common type of these expressions, and also the
   input type of the arrow
 * a CoreExpr for an arrow built by combining the translated command
   bodies with |||.



Translation of command judgements of the form

        D |-a do { ss } : t



A statement maps one local environment to another, and is represented
as an arrow from one tuple type to another.  A statement sequence is
translated to a composition of such arrows.



A sequence of statements (as in a rec) is desugared to an arrow between
two environments (no stack)


### Note: Dictionary binders in ConPatOut

The following functions to collect value variables from patterns are
copied from HsUtils, with one change: we also collect the dictionary
bindings (pat_binds) from ConPatOut.  We need them for cases like

h :: Arrow a => Int -> a (Int,Int) Int
h x = proc (y,z) -> case compare x y of
                GT -> returnA -< z+x

The type checker turns the case into

                case compare x y of
                  GT { p77 = plusInt } -> returnA -< p77 z x

Here p77 is a local binding for the (+) operation.

See comments in HsUtils for why the other version does not include
these bindings.
