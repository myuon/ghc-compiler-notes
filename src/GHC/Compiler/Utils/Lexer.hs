module GHC.Compiler.Utils.Lexer where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class

import           Data.Conduit
import           Data.Conduit.Combinators  (sinkList)

import           DynFlags                  (DynFlags)

import           FastString                (mkFastString)

import qualified Lexer
import           Lexer                     (ParseResult(..), Token(..))

import qualified Outputable

import           Pretty                    (Doc)

import           SrcLoc

import qualified StringBuffer

type Parser = Lexer.P

newtype ParseFailed = ParseFailed { pFailedMsg :: [(SrcSpan, Doc)] }
  deriving Show

fromParseResult :: DynFlags -> ParseResult a -> Either ParseFailed a
fromParseResult _ (POk _ x) = Right x
fromParseResult dflags (PFailed _ s d) = Left $ ParseFailed [(s, Outputable.runSDoc d ctx)]
  where
    ctx = Outputable.initSDocContext dflags $ Outputable.defaultErrStyle dflags

runParser :: Parser a
          -> DynFlags
          -> StringBuffer.StringBuffer
          -> RealSrcLoc
          -> Either ParseFailed a
runParser p dflags buf loc = fromPR $ Lexer.unP p $ Lexer.mkPState dflags buf loc
  where
    fromPR = fromParseResult dflags

runParserFromString :: Parser a -> DynFlags -> String -> Either ParseFailed a
runParserFromString p dflags str = runParser p dflags buf loc
  where
    buf = StringBuffer.stringToStringBuffer str

    loc = mkRealSrcLoc (mkFastString "") 1 1

runParserFromFile :: MonadIO m => Parser a -> DynFlags -> String -> m (Either ParseFailed a)
runParserFromFile p dflags fn = do
  buf <- liftIO $ StringBuffer.hGetStringBuffer fn
  pure $ runParser p dflags buf loc
  where
    loc = mkRealSrcLoc (mkFastString fn) 1 1

pTokenConsumer :: ConduitT i (Located Token) Parser ()
pTokenConsumer = go
  where
    go = do
      t <- lift $ Lexer.lexer False pure
      yield t
      case unLoc t of
        ITeof -> pure ()
        _     -> go

pTokenize :: Parser [Located Token]
pTokenize = runConduit $ pTokenConsumer .| sinkList
