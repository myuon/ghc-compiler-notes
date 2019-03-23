Note [Order of tests]
~~~~~~~~~~~~~~~~~~~~~~~~~
(See #3065, #5161.) Suppose we had a definition like:

    quot x y
     | y == 0                     = divZeroError
     | x == minBound && y == (-1) = overflowError
     | otherwise                  = x `primQuot` y

Note in particular that the
    x == minBound
test comes before the
    y == (-1)
test.

this expands to something like:

    case y of
    0 -> divZeroError
    _ -> case x of
         -9223372036854775808 ->
             case y of
             -1 -> overflowError
             _ -> x `primQuot` y
         _ -> x `primQuot` y

Now if we have the call (x `quot` 2), and quot gets inlined, then we get:

    case 2 of
    0 -> divZeroError
    _ -> case x of
         -9223372036854775808 ->
             case 2 of
             -1 -> overflowError
             _ -> x `primQuot` 2
         _ -> x `primQuot` 2

which simplifies to:

    case x of
    -9223372036854775808 -> x `primQuot` 2
    _                    -> x `primQuot` 2

Now we have a case with two identical branches, which would be
eliminated (assuming it doesn't affect strictness, which it doesn't in
this case), leaving the desired:

    x `primQuot` 2

except in the minBound branch we know what x is, and GHC cleverly does
the division at compile time, giving:

    case x of
    -9223372036854775808 -> -4611686018427387904
    _                    -> x `primQuot` 2

So instead we use a definition like:

    quot x y
     | y == 0                     = divZeroError
     | y == (-1) && x == minBound = overflowError
     | otherwise                  = x `primQuot` y

which gives us:

    case y of
    0 -> divZeroError
    -1 ->
        case x of
        -9223372036854775808 -> overflowError
        _ -> x `primQuot` y
    _ -> x `primQuot` y

for which our call (x `quot` 2) expands to:

    case 2 of
    0 -> divZeroError
    -1 ->
        case x of
        -9223372036854775808 -> overflowError
        _ -> x `primQuot` 2
    _ -> x `primQuot` 2

which simplifies to:

    x `primQuot` 2

as required.



But we now have the same problem with a constant numerator: the call
(2 `quot` y) expands to

    case y of
    0 -> divZeroError
    -1 ->
        case 2 of
        -9223372036854775808 -> overflowError
        _ -> 2 `primQuot` y
    _ -> 2 `primQuot` y

which simplifies to:

    case y of
    0 -> divZeroError
    -1 -> 2 `primQuot` y
    _ -> 2 `primQuot` y

which simplifies to:

    case y of
    0 -> divZeroError
    -1 -> -2
    _ -> 2 `primQuot` y


However, constant denominators are more common than constant numerators,
so the
    y == (-1) && x == minBound
order gives us better code in the common case.
