Note [Errors in base]
~~~~~~~~~~~~~~~~~~~~~
As of base-4.9.0.0, `error` produces a stack trace alongside the
error message using the HasCallStack machinery. This provides
a partial stack trace, containing the call-site of each function
with a HasCallStack constraint.

In base, however, the only functions that have such constraints are
error and undefined, so the stack traces from partial functions in
base will never contain a call-site in user code. Instead we'll
usually just get the actual call to error. Base functions already
have a good habit of providing detailed error messages, including the
name of the offending partial function, so the partial stack-trace
does not provide any extra information, just noise. Thus, we export
the callstack-aware error, but within base we use the
errorWithoutStackTrace variant for more hygienic error messages.
