module Data.Text.Monadic where

import Data.Text.Internal
import Data.Text.Unsafe
import Data.Text () -- instance Monoid Text


takeWhileM :: Monad m => (Char -> m Bool) -> Text -> m Text
takeWhileM p t@(Text arr off len) = loop 0
  where
    loop !i
      | i >= len  = pure t
      | otherwise = do
        let Iter c d = iter t i
        p c >>= \case
          True  -> loop $ i + d
          False -> pure $ text arr off i
{-# INLINE takeWhileM #-}

dropWhileM :: Monad m => (Char -> m Bool) -> Text -> m Text
dropWhileM p t@(Text arr off len) = loop 0 0
  where
    loop !i !l
      | l >= len  = pure mempty
      | otherwise = do
        let Iter c d = iter t i
        p c >>= \case
          True  -> loop (i + d) (l + d)
          False -> pure $ text arr (off + i) (len - l)
{-# INLINE dropWhileM #-}
