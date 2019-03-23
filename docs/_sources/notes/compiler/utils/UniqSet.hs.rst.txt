Note [UniqSet invariant]
~~~~~~~~~~~~~~~~~~~~~~~~~
UniqSet has the following invariant:
  The keys in the map are the uniques of the values
It means that to implement mapUniqSet you have to update
both the keys and the values.
