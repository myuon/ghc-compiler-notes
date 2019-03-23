Note [Eta expansion]
~~~~~~~~~~~~~~~~~~~~
When matching
     (\x.M) ~ N
we rename x to x' with, where x' is not in scope in
either term.  Then we want to behave as if we'd seen
     (\x'.M) ~ (\x'.N x')
Since x' isn't in scope in N, the form (\x'. N x') doesn't
capture any variables in N.  But we must nevertheless extend
the envR with a binding [x' -> x'], to support the occurs check.
For example, if we don't do this, we can get silly matches like
        forall a.  (\y.a)  ~   v
succeeding with [a -> v y], which is bogus of course.


