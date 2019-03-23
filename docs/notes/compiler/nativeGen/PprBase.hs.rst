`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/nativeGen/PprBase.hs>`_

Note [Embedding large binary blobs]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To embed a blob of binary data (e.g. an UTF-8 encoded string) into the generated
code object, we have several options:

   1. Generate a ".byte" directive for each byte. This is what was done in the past
      (see Note [Pretty print ASCII when AsmCodeGen]).

   2. Generate a single ".string"/".asciz" directive for the whole sequence of
      bytes. Bytes in the ASCII printable range are rendered as characters and
      other values are escaped (e.g., "\t", "\077", etc.).

   3. Create a temporary file into which we dump the binary data and generate a
      single ".incbin" directive. The assembler will include the binary file for
      us in the generated output object.

Now the code generator uses either (2) or (3), depending on the binary blob
size.  Using (3) for small blobs adds too much overhead (see benchmark results
in #16190), so we only do it when the size is above a threshold (500K at the
time of writing).

The threshold is configurable via the `-fbinary-blob-threshold` flag.



Note [Pretty print ASCII when AsmCodeGen]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Previously, when generating assembly code, we created SDoc with
`(ptext . sLit)` for every bytes in literal bytestring, then
combine them using `hcat`.

When handling literal bytestrings with millions of bytes,
millions of SDoc would be created and to combine, leading to
high memory usage.

Now we escape the given bytestring to string directly and construct
SDoc only once. This improvement could dramatically decrease the
memory allocation from 4.7GB to 1.3GB when embedding a 3MB literal
string in source code. See #14741 for profiling results.
----------------------------------------------------------------------------
Printing section headers.

If -split-section was specified, include the suffix label, otherwise just
print the section type. For Darwin, where subsections-for-symbols are
used instead, only print section type.

For string literals, additional flags are specified to enable merging of
identical strings in the linker. With -split-sections each string also gets
a unique section to allow strings from unused code to be GC'd.

