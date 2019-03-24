`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/iface/BinIface.hs>`_

====================
compiler/iface/BinIface.hs.rst
====================

Note [Symbol table representation of names]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

An occurrence of a name in an interface file is serialized as a single 32-bit
word. The format of this word is:
 00xxxxxx xxxxxxxx xxxxxxxx xxxxxxxx
  A normal name. x is an index into the symbol table
 10xxxxxx xxyyyyyy yyyyyyyy yyyyyyyy
  A known-key name. x is the Unique's Char, y is the int part. We assume that
  all known-key uniques fit in this space. This is asserted by
  PrelInfo.knownKeyNamesOkay.

During serialization we check for known-key things using isKnownKeyName.
During deserialization we use lookupKnownKeyName to get from the unique back
to its corresponding Name.
See Note [Symbol table representation of names]

