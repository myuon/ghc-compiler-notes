[[src]](https://github.com/ghc/ghc/tree/master/compiler/utils/Encoding.hs)

This is the main name-encoding and decoding function.  It encodes any
string into a string that is acceptable as a C name.  This is done
right before we emit a symbol name into the compiled C or asm code.
Z-encoding of strings is cached in the FastString interface, so we
never encode the same string more than once.

The basic encoding scheme is this.

* Tuples (,,,) are coded as Z3T

* Alphabetic characters (upper and lower) and digits
        all translate to themselves;
        except 'Z', which translates to 'ZZ'
        and    'z', which translates to 'zz'
  We need both so that we can preserve the variable/tycon distinction

* Most other printable characters translate to 'zx' or 'Zx' for some
        alphabetic character x

* The others translate as 'znnnU' where 'nnn' is the decimal number
        of the character

        Before          After
        --------------------------
        Trak            Trak
        foo_wib         foozuwib
        >               zg
        >1              zg1
        foo#            foozh
        foo##           foozhzh
        foo##1          foozhzh1
        fooZ            fooZZ
        :+              ZCzp
        ()              Z0T     0-tuple
        (,,,,)          Z5T     5-tuple
        (# #)           Z1H     unboxed 1-tuple (note the space)
        (#,,,,#)        Z5H     unboxed 5-tuple
                (NB: There is no Z1T nor Z0H.)


pprTrace "decode_upper" (char ch)

pprTrace "decode_lower" (char ch)


Tuples are encoded as
        Z3T or Z3H
for 3-tuples or unboxed 3-tuples respectively.  No other encoding starts
        Z<digit>

* "(# #)" is the tycon for an unboxed 1-tuple (not 0-tuple)
  There are no unboxed 0-tuples.

* "()" is the tycon for a boxed 0-tuple.
  There are no boxed 1-tuples.


# Base 62


### Note: Base 62 encoding 128-bit integers

Instead of base-62 encoding a single 128-bit integer
(ceil(21.49) characters), we'll base-62 a pair of 64-bit integers
(2 * ceil(10.75) characters).  Luckily for us, it's the same number of
characters!
