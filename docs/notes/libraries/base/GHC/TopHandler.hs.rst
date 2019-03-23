Note [rts_setMainThread must be called unsafely]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rts_setMainThread must be called as unsafe, because it
dereferences the Weak# and manipulates the raw Haskell value
behind it.  Therefore, it must not race with a garbage collection.


Note [rts_setMainThread has an unsound type]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

'rts_setMainThread' is imported with type Weak# ThreadId -> IO (),
but this is an unsound type for it: it grabs the /key/ of the
'Weak#' object, which isn't tracked by the type at all.
That this works at all is a consequence of the fact that
'mkWeakThreadId' produces a 'Weak#' with a 'ThreadId#' as the key
This is fairly robust, in that 'mkWeakThreadId' wouldn't work
otherwise, but it still is sufficiently non-trivial to justify an
ASSERT in rts/TopHandler.c.
see Note [rts_setMainThread must be called unsafely] and
Note [rts_setMainThread has an unsound type]


Note [Disaster with iconv]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When using iconv, it's possible for things like iconv_open to fail in
restricted environments (like an initram or restricted container), but
when this happens the error raised inevitably calls `peekCString`,
which depends on the users locale, which depends on using
`iconv_open`... which causes an infinite loop.

This occurrence is also known as tickets #10298 and #7695. So to work
around it we just set _another_ error handler and bail directly by
calling the RTS, without iconv at all.
try to flush stdout/stderr, but don't worry if we fail
(these handles might have errors, and we don't want to go into
an infinite loop).
