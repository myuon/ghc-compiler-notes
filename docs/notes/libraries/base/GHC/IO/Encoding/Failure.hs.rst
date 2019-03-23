`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/libraries/base/GHC/IO/Encoding/Failure.hs>`_

Note [Roundtripping]
~~~~~~~~~~~~~~~~~~~~

Roundtripping is based on the ideas of PEP383.

We used to use the range of private-use characters from 0xEF80 to
0xEFFF designated for "encoding hacks" by the ConScript Unicode Registery
to encode these characters.

However, people didn't like this because it means we don't get
guaranteed roundtripping for byte sequences that look like a UTF-8
encoded codepoint 0xEFxx.

So now like PEP383 we use lone surrogate codepoints 0xDCxx to escape
undecodable bytes, even though that may confuse Unicode processing
software written in Haskell. This guarantees roundtripping because
unicode input that includes lone surrogate codepoints is invalid by
definition.


When we used private-use characters there was a technical problem when it
came to encoding back to bytes using iconv. The iconv code will not fail when
it tries to encode a private-use character (as it would if trying to encode
a surrogate), which means that we wouldn't get a chance to replace it
with the byte we originally escaped.

To work around this, when filling the buffer to be encoded (in
writeBlocks/withEncodedCString/newEncodedCString), we replaced the
private-use characters with lone surrogates again! Likewise, when
reading from a buffer (unpack/unpack_nl/peekEncodedCString) we had
to do the inverse process.

The user of String would never see these lone surrogates, but it
ensured that iconv will throw an error when encountering them.  We
used lone surrogates in the range 0xDC00 to 0xDCFF for this purpose.

