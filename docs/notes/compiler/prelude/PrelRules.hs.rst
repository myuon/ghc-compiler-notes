Note [Constant folding]
~~~~~~~~~~~~~~~~~~~~~~~
primOpRules generates a rewrite rule for each primop
These rules do what is often called "constant folding"
E.g. the rules for +# might say
        4 +# 5 = 9
Well, of course you'd need a lot of rules if you did it
like that, so we use a BuiltinRule instead, so that we
can match in any two literal values.  So the rule is really
more like
        (Lit x) +# (Lit y) = Lit (x+#y)
where the (+#) on the rhs is done at compile time

That is why these rules are built in here.


Note [Rules for floating-point comparisons]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We need different rules for floating-point values because for floats
it is not true that x = x (for NaNs); so we do not want the equal_rule
rule that mkRelOpRule uses.

Note also that, in the case of equality/inequality, we do /not/
want to switch to a case-expression.  For example, we do not want
to convert
   case (eqFloat# x 3.8#) of
     True -> this
     False -> that
to
  case x of
    3.8#::Float# -> this
    _            -> that
See #9238.  Reason: comparing floating-point values for equality
delicate, and we don't want to implement that delicacy in the code for
case expressions.  So we make it an invariant of Core that a case
expression never scrutinises a Float# or Double#.

This transformation is what the litEq rule does;
see Note [The litEq rule: converting equality to case].
So we /refrain/ from using litEq for mkFloatingRelOpRule.


Note [The litEq rule: converting equality to case]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
This stuff turns
     n ==# 3#
into
     case n of
       3# -> True
       m  -> False

This is a Good Thing, because it allows case-of case things
to happen, and case-default absorption to happen.  For
example:

     if (n ==# 3#) || (n ==# 4#) then e1 else e2
will transform to
     case n of
       3# -> e1
       4# -> e1
       m  -> e2
(modulo the usual precautions to avoid duplicating e1)


Note [Guarding against silly shifts]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Consider this code:

  import Data.Bits( (.|.), shiftL )
  chunkToBitmap :: [Bool] -> Word32
  chunkToBitmap chunk = foldr (.|.) 0 [ 1 `shiftL` n | (True,n) <- zip chunk [0..] ]

This optimises to:
Shift.$wgo = \ (w_sCS :: GHC.Prim.Int#) (w1_sCT :: [GHC.Types.Bool]) ->
    case w1_sCT of _ {
      [] -> 0##;
      : x_aAW xs_aAX ->
        case x_aAW of _ {
          GHC.Types.False ->
            case w_sCS of wild2_Xh {
              __DEFAULT -> Shift.$wgo (GHC.Prim.+# wild2_Xh 1) xs_aAX;
              9223372036854775807 -> 0## };
          GHC.Types.True ->
            case GHC.Prim.>=# w_sCS 64 of _ {
              GHC.Types.False ->
                case w_sCS of wild3_Xh {
                  __DEFAULT ->
                    case Shift.$wgo (GHC.Prim.+# wild3_Xh 1) xs_aAX of ww_sCW { __DEFAULT ->
                      GHC.Prim.or# (GHC.Prim.narrow32Word#
                                      (GHC.Prim.uncheckedShiftL# 1## wild3_Xh))
                                   ww_sCW
                     };
                  9223372036854775807 ->
                    GHC.Prim.narrow32Word#
!!!!-->                  (GHC.Prim.uncheckedShiftL# 1## 9223372036854775807)
                };
              GHC.Types.True ->
                case w_sCS of wild3_Xh {
                  __DEFAULT -> Shift.$wgo (GHC.Prim.+# wild3_Xh 1) xs_aAX;
                  9223372036854775807 -> 0##
                } } } }

Note the massive shift on line "!!!!".  It can't happen, because we've checked
that w < 64, but the optimiser didn't spot that. We DO NOT want to constant-fold this!
Moreover, if the programmer writes (n `uncheckedShiftL` 9223372036854775807), we
can't constant fold it, but if it gets to the assember we get
     Error: operand type mismatch for `shl'

So the best thing to do is to rewrite the shift with a call to error,
when the second arg is stupid.

There are two cases:

- Shifting fixed-width things: the primops ISll, Sll, etc
  These are handled by shiftRule.

  We are happy to shift by any amount up to wordSize but no more.

- Shifting Integers: the function shiftLInteger, shiftRInteger
  from the 'integer' library.   These are handled by rule_shift_op,
  and match_Integer_shift_op.

  Here we could in principle shift by any amount, but we arbitary
  limit the shift to 4 bits; in particualr we do not want shift by a
  huge amount, which can happen in code like that above.

The two cases are more different in their code paths that is comfortable,
but that is only a historical accident.




Note [Strength reduction]
~~~~~~~~~~~~~~~~~~~~~~~~~

This rule turns floating point multiplications of the form 2.0 * x and
x * 2.0 into x + x addition, because addition costs less than multiplication.
See #7116


Note [What's true and false]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

trueValInt and falseValInt represent true and false values returned by
comparison primops for Char, Int, Word, Integer, Double, Float and Addr.
True is represented as an unboxed 1# literal, while false is represented
as 0# literal.
We still need Bool data constructors (True and False) to use in a rule
for constant folding of equal Strings


Note [tagToEnum#]
~~~~~~~~~~~~~~~~~
Nasty check to ensure that tagToEnum# is applied to a type that is an
enumeration TyCon.  Unification may refine the type later, but this
check won't see that, alas.  It's crude but it works.

Here's are two cases that should fail
        f :: forall a. a
        f = tagToEnum# 0        -- Can't do tagToEnum# at a type variable

        g :: Int
        g = tagToEnum# 0        -- Int is not an enumeration

We used to make this check in the type inference engine, but it's quite
ugly to do so, because the delayed constraint solving means that we don't
really know what's going on until the end. It's very much a corner case
because we don't expect the user to call tagToEnum# at all; we merely
generate calls in derived instances of Enum.  So we compromise: a
rewrite rule rewrites a bad instance of tagToEnum# to an error call,
and emits a warning.


Note [dataToTag# magic]
~~~~~~~~~~~~~~~~~~~~~~~~~~
The primop dataToTag# is unusual because it evaluates its argument.
Only `SeqOp` shares that property.  (Other primops do not do anything
as fancy as argument evaluation.)  The special handling for dataToTag#
is:

* CoreUtils.exprOkForSpeculation has a special case for DataToTagOp,
  (actually in app_ok).  Most primops with lifted arguments do not
  evaluate those arguments, but DataToTagOp and SeqOp are two
  exceptions.  We say that they are /never/ ok-for-speculation,
  regardless of the evaluated-ness of their argument.
  See CoreUtils Note [exprOkForSpeculation and SeqOp/DataToTagOp]

* There is a special case for DataToTagOp in StgCmmExpr.cgExpr,
  that evaluates its argument and then extracts the tag from
  the returned value.

* An application like (dataToTag# (Just x)) is optimised by
  dataToTagRule in PrelRules.

* A case expression like
     case (dataToTag# e) of <alts>
  gets transformed t
     case e of <transformed alts>
  by PrelRules.caseRules; see Note [caseRules for dataToTag]

See #15696 for a long saga.




Note [seq# magic]
~~~~~~~~~~~~~~~~~~~~
The primop
   seq# :: forall a s . a -> State# s -> (# State# s, a #)

is /not/ the same as the Prelude function seq :: a -> b -> b
as you can see from its type.  In fact, seq# is the implementation
mechanism for 'evaluate'

   evaluate :: a -> IO a
   evaluate a = IO $ \s -> seq# a s

The semantics of seq# is
  * evaluate its first argument
  * and return it

Things to note

* Why do we need a primop at all?  That is, instead of
      case seq# x s of (# x, s #) -> blah
  why not instead say this?
      case x of { DEFAULT -> blah)

  Reason (see #5129): if we saw
    catch# (\s -> case x of { DEFAULT -> raiseIO# exn s }) handler

  then we'd drop the 'case x' because the body of the case is bottom
  anyway. But we don't want to do that; the whole /point/ of
  seq#/evaluate is to evaluate 'x' first in the IO monad.

  In short, we /always/ evaluate the first argument and never
  just discard it.

* Why return the value?  So that we can control sharing of seq'd
  values: in
     let x = e in x `seq` ... x ...
  We don't want to inline x, so better to represent it as
       let x = e in case seq# x RW of (# _, x' #) -> ... x' ...
  also it matches the type of rseq in the Eval monad.

Implementing seq#.  The compiler has magic for SeqOp in

- PrelRules.seqRule: eliminate (seq# <whnf> s)

- StgCmmExpr.cgExpr, and cgCase: special case for seq#

- CoreUtils.exprOkForSpeculation;
  see Note [exprOkForSpeculation and SeqOp/DataToTagOp] in CoreUtils

- Simplify.addEvals records evaluated-ness for the result; see
  Note [Adding evaluatedness info to pattern-bound variables]
  in Simplify


Note [Scoping for Builtin rules]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
When compiling a (base-package) module that defines one of the
functions mentioned in the RHS of a built-in rule, there's a danger
that we'll see

        f = ...(eq String x)....

        ....and lower down...

        eqString = ...

Then a rewrite would give

        f = ...(eqString x)...
        ....and lower down...
        eqString = ...

and lo, eqString is not in scope.  This only really matters when we get to code
generation.  With -O we do a GlomBinds step that does a new SCC analysis on the whole
set of bindings, which sorts out the dependency.  Without -O we don't do any rule
rewriting so again we are fine.

(This whole thing doesn't show up for non-built-in rules because their dependencies
are explicit.)


Note [Rewriting bitInteger]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
For most types the bitInteger operation can be implemented in terms of shifts.
The integer-gmp package, however, can do substantially better than this if
allowed to provide its own implementation. However, in so doing it previously lost
constant-folding (see #8832). The bitInteger rule above provides constant folding
specifically for this function.

There is, however, a bit of trickiness here when it comes to ranges. While the
AST encodes all integers as Integers, `bit` expects the bit
index to be given as an Int. Hence we coerce to an Int in the rule definition.
This will behave a bit funny for constants larger than the word size, but the user
should expect some funniness given that they will have at very least ignored a
warning in this case.


Note [caseRules for tagToEnum]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
We want to transform
   case tagToEnum x of
     False -> e1
     True  -> e2
into
   case x of
     0# -> e1
     1# -> e2

This rule eliminates a lot of boilerplate. For
  if (x>y) then e2 else e1
we generate
  case tagToEnum (x ># y) of
    False -> e1
    True  -> e2
and it is nice to then get rid of the tagToEnum.

Beware (#14768): avoid the temptation to map constructor 0 to
DEFAULT, in the hope of getting this
  case (x ># y) of
    DEFAULT -> e1
    1#      -> e2
That fails utterly in the case of
   data Colour = Red | Green | Blue
   case tagToEnum x of
      DEFAULT -> e1
      Red     -> e2

We don't want to get this!
   case x of
      DEFAULT -> e1
      DEFAULT -> e2

Instead, we deal with turning one branch into DEFAULT in SimplUtils
(add_default in mkCase3).



Note [caseRules for dataToTag]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
See also Note [dataToTag#] in primpops.txt.pp

We want to transform
  case dataToTag x of
    DEFAULT -> e1
    1# -> e2
into
  case x of
    DEFAULT -> e1
    (:) _ _ -> e2

Note the need for some wildcard binders in
the 'cons' case.

For the time, we only apply this transformation when the type of `x` is a type
headed by a normal tycon. In particular, we do not apply this in the case of a
data family tycon, since that would require carefully applying coercion(s)
between the data family and the data family instance's representation type,
which caseRules isn't currently engineered to handle (#14680).



Note [Unreachable caseRules alternatives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Take care if we see something like
  case dataToTag x of
    DEFAULT -> e1
    -1# -> e2
    100 -> e3
because there isn't a data constructor with tag -1 or 100. In this case the
out-of-range alterantive is dead code -- we know the range of tags for x.

Hence caseRules returns (AltCon -> Maybe AltCon), with Nothing indicating
an alternative that is unreachable.

You may wonder how this can happen: check out #15436.
