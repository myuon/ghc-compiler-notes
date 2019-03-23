Note [Numeric Stability of Enumerating Floating Numbers]
-----------------------------------------------------------
When enumerate floating numbers, we could add the increment to the last number
at every run (as what we did previously):

    numericEnumFrom n =  n `seq` (n : numericEnumFrom (n + 1))

This approach is concise and really fast, only needs an addition operation.
However when a floating number is large enough, for `n`, `n` and `n+1` will
have the same binary representation. For example (all number has type
`Double`):

    9007199254740990                 is: 0x433ffffffffffffe
    9007199254740990 + 1             is: 0x433fffffffffffff
    (9007199254740990 + 1) + 1       is: 0x4340000000000000
    ((9007199254740990 + 1) + 1) + 1 is: 0x4340000000000000

When we evaluate ([9007199254740990..9007199254740991] :: Double), we would
never reach the condition in `numericEnumFromTo`

    9007199254740990 + 1 + 1 + ... > 9007199254740991 + 1/2

We would fall into infinite loop (as reported in #15081).

To remedy the situation, we record the number of `1` that needed to be added
to the start number, rather than increasing `1` at every time. This approach
can improvement the numeric stability greatly at the cost of a multiplication.

Furthermore, we use the type of the enumerated number, `Fractional a => a`,
as the type of multiplier. In rare situations, the multiplier could be very
large and will lead to the enumeration to infinite loop, too, which should
be very rare. Consider the following example:

    [1..9007199254740994]

We could fix that by using an Integer as multiplier but we don't do that.
The benchmark on T7954.hs shows that this approach leads to significant
degeneration on performance (33% increase allocation and 300% increase on
elapsed time).

See #15081 and Phab:D4650 for the related discussion about this problem.
------------------------------------------------------------
 Instances for Int
------------------------------------------------------------


Note [Integer division constant folding]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Constant folding of quot, rem, div, mod, divMod and quotRem for
Integer arguments depends crucially on inlining. Constant folding
rules defined in compiler/prelude/PrelRules.hs trigger for
quotInteger, remInteger and so on. So if calls to quot, rem and so on
were not inlined the rules would not fire. The rules would also not
fire if calls to quotInteger and so on were inlined, but this does not
happen because they are all marked with NOINLINE pragma - see documentation
of integer-gmp or integer-simple.
