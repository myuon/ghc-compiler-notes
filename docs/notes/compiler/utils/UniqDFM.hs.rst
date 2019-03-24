`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/utils/UniqDFM.hs>`_

====================
compiler/utils/UniqDFM.hs.rst
====================

Note [Deterministic UniqFM]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
A @UniqDFM@ is just like @UniqFM@ with the following additional
property: the function `udfmToList` returns the elements in some
deterministic order not depending on the Unique key for those elements.

If the client of the map performs operations on the map in deterministic
order then `udfmToList` returns them in deterministic order.

There is an implementation cost: each element is given a serial number
as it is added, and `udfmToList` sorts it's result by this serial
number. So you should only use `UniqDFM` if you need the deterministic
property.

`foldUDFM` also preserves determinism.

Normal @UniqFM@ when you turn it into a list will use
Data.IntMap.toList function that returns the elements in the order of
the keys. The keys in @UniqFM@ are always @Uniques@, so you end up with
with a list ordered by @Uniques@.
The order of @Uniques@ is known to be not stable across rebuilds.
See Note [Unique Determinism] in Unique.


There's more than one way to implement this. The implementation here tags
every value with the insertion time that can later be used to sort the
values when asked to convert to a list.

An alternative would be to have

.. code-block:: haskell

  data UniqDFM ele = UDFM (M.IntMap ele) [ele]

where the list determines the order. This makes deletion tricky as we'd
only accumulate elements in that list, but makes merging easier as you
can just merge both structures independently.
Deletion can probably be done in amortized fashion when the size of the
list is twice the size of the set.


Note [Overflow on plusUDFM]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
There are multiple ways of implementing plusUDFM.
The main problem that needs to be solved is overlap on times of
insertion between different keys in two maps.
Consider:

A = fromList [(a, (x, 1))]
B = fromList [(b, (y, 1))]

If you merge them naively you end up with:

C = fromList [(a, (x, 1)), (b, (y, 1))]

Which loses information about ordering and brings us back into
non-deterministic world.

The solution I considered before would increment the tags on one of the
sets by the upper bound of the other set. The problem with this approach
is that you'll run out of tags for some merge patterns.
Say you start with A with upper bound 1, you merge A with A to get A' and
the upper bound becomes 2. You merge A' with A' and the upper bound
doubles again. After 64 merges you overflow.
This solution would have the same time complexity as plusUFM, namely O(n+m).

The solution I ended up with has time complexity of
O(m log m + m * min (n+m, W)) where m is the smaller set.
It simply inserts the elements of the smaller set into the larger
set in the order that they were inserted into the smaller set. That's
O(m log m) for extracting the elements from the smaller set in the
insertion order and O(m * min(n+m, W)) to insert them into the bigger
set.

