`[source] <https://gitlab.haskell.org/ghc/ghc/tree/master/compiler/utils/MonadUtils.hs>`_

====================
compiler/utils/MonadUtils.hs.rst
====================

Note [Inline @zipWithNM@ functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The inline principle for 'zipWith3M', 'zipWith4M' and 'zipWith3M_' is the same
as for 'zipWithM' and 'zipWithM_' in "Control.Monad", see
Note [Fusion for zipN/zipWithN] in GHC/List.hs for more details.

The 'zipWithM'/'zipWithM_' functions are inlined so that the `zipWith` and
`sequenceA` functions with which they are defined have an opportunity to fuse.

Furthermore, 'zipWith3M'/'zipWith4M' and 'zipWith3M_' have been explicitly
rewritten in a non-recursive way similarly to 'zipWithM'/'zipWithM_', and for
more than just uniformity: after [D5241](https://phabricator.haskell.org/D5241)
for issue #14037, all @zipN@/@zipWithN@ functions fuse, meaning
'zipWith3M'/'zipWIth4M' and 'zipWith3M_'@ now behave like 'zipWithM' and
'zipWithM_', respectively, with regards to fusion.

As such, since there are not any differences between 2-ary 'zipWithM'/
'zipWithM_' and their n-ary counterparts below aside from the number of
arguments, the `INLINE` pragma should be replicated in the @zipWithNM@
functions below as well.



Note [Inline @mapAndUnzipNM@ functions]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The inline principle is the same as 'mapAndUnzipM' in "Control.Monad".
The 'mapAndUnzipM' function is inlined so that the `unzip` and `traverse`
functions with which it is defined have an opportunity to fuse, see
Note [Inline @unzipN@ functions] in Data/OldList.hs for more details.

Furthermore, the @mapAndUnzipNM@ functions have been explicitly rewritten in a
non-recursive way similarly to 'mapAndUnzipM', and for more than just
uniformity: after [D5249](https://phabricator.haskell.org/D5249) for Trac
ticket #14037, all @unzipN@ functions fuse, meaning 'mapAndUnzip3M',
'mapAndUnzip4M' and 'mapAndUnzip5M' now behave like 'mapAndUnzipM' with regards
to fusion.

As such, since there are not any differences between 2-ary 'mapAndUnzipM' and
its n-ary counterparts below aside from the number of arguments, the `INLINE`
pragma should be replicated in the @mapAndUnzipNM@ functions below as well.


