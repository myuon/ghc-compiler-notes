Note [toIntegralSized optimization]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The code in 'toIntegralSized' relies on GHC optimizing away statically
decidable branches.

If both integral types are statically known, GHC will be able optimize the
code significantly (for @-O1@ and better).

For instance (as of GHC 7.8.1) the following definitions:

> w16_to_i32 = toIntegralSized :: Word16 -> Maybe Int32
>
> i16_to_w16 = toIntegralSized :: Int16 -> Maybe Word16

are translated into the following (simplified) /GHC Core/ language:

> w16_to_i32 = \x -> Just (case x of _ { W16# x# -> I32# (word2Int# x#) })
>
> i16_to_w16 = \x -> case eta of _
>   { I16# b1 -> case tagToEnum# (<=# 0 b1) of _
>       { False -> Nothing
>       ; True -> Just (W16# (narrow16Word# (int2Word# b1)))
>       }
>   }
