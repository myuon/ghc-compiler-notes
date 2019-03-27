module Data.Text.Extra where

import qualified Data.Char          as Char
import           Data.Text          () -- instance Monoid Text
import           Data.Text.Internal
import           Data.Text.Unsafe

takeWhileM :: Monad m => (Char -> m Bool) -> Text -> m Text
takeWhileM p t@(Text arr off len) = loop 0
  where
    loop !i
      | i >= len = pure t
      | otherwise = do
        let Iter c d = iter t i
        p c >>= \case
          True  -> loop $ i + d
          False -> pure $ text arr off i

{-# INLINE takeWhileM #-}

dropWhileM :: Monad m => (Char -> m Bool) -> Text -> m Text
dropWhileM p t@(Text arr off len) = loop 0
  where
    loop !i
      | i >= len = pure mempty
      | otherwise = do
        let Iter c d = iter t i
        p c >>= \case
          True  -> loop (i + d)
          False -> pure $ text arr (off + i) (len - i)

{-# INLINE dropWhileM #-}

stripEmptyLinesStart :: Text -> Text
stripEmptyLinesStart t@(Text arr off len) = loop 0 0
  where
    loop !ri !i
      | i >= len = loopEnd ri
      | otherwise = let Iter c d = iter t i in if
        | c == '\n' -> let !ni = i + d in loop ni ni
        | Char.isSpace c -> loop ri (i + d)
        | otherwise -> loopEnd ri

    loopEnd ri
      | ri >= len = mempty
      | otherwise = text arr (off + ri) (len - ri)

{-# INLINE stripEmptyLinesStart #-}

stripEmptyLinesEnd :: Text -> Text
stripEmptyLinesEnd t@(Text arr off len) = let !i0 = len - 1 in loop i0 i0
  where
    loop !ri !i
      | i < 0 = loopEnd i
      | otherwise = let (c, d) = reverseIter t i in if
        | c == '\n' -> loop i (i + d)
        | Char.isSpace c -> loop ri (i + d)
        | otherwise -> loopEnd ri

    loopEnd ri
      | ri < 0 = mempty
      | otherwise = text arr off (ri + 1)

{-# INLINE stripEmptyLinesEnd #-}

stripEmptyLines :: Text -> Text
stripEmptyLines = stripEmptyLinesEnd . stripEmptyLinesStart

{-# INLINE stripEmptyLines #-}
