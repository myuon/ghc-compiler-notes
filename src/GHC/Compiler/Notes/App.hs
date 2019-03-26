{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module GHC.Compiler.Notes.App where

import           Capability.Accessors
import           Capability.Reader

import           Control.Monad
import           Control.Monad.Catch

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader (ReaderT(..))

import           Control.Monad.Trans.State

import           Data.Kind

import qualified Data.Text                  as Text

import qualified DynFlags

import           GHC                        (getSessionDynFlags, runGhc)

import           GHC.Compiler.Notes.Types
import           GHC.Generics

import           GHC.LanguageExtensions     (Extension)
import qualified GHC.Paths

import qualified Packages

import           SrcLoc

data AppContext = AppContext { envDynFlags :: DynFlags.DynFlags
                             , envSourceResourceGetter :: FilePath -> Maybe SrcSpan -> Text.Text
                             }
  deriving Generic

type OnOff a = (Bool, a)

data AppContextCommon = AppContextCommon { sFlagsExtensions         :: [OnOff Extension]
                                         , sFlagsGeneralOptions     :: [OnOff DynFlags.GeneralFlag]
                                         , sFlagsGlobalIncludePaths :: [String]
                                         }

ghcInitDynFlags :: MonadIO m => m DynFlags.DynFlags
ghcInitDynFlags = do
  dflags0 <- liftIO $ runGhc (Just GHC.Paths.libdir) getSessionDynFlags
  (dflags1, _) <- liftIO $ Packages.initPackages dflags0
  pure dflags1

appContext :: MonadIO m => AppContextCommon -> m AppContext
appContext AppContextCommon{..} = do
  defDflags <- ghcInitDynFlags
  let dflags = execState dflagsUpdater defDflags

  return $ AppContext { envDynFlags = dflags, envSourceResourceGetter = gitlabResourceGetter }
  where
    dflagsUpdater = do
      forM_ sFlagsGeneralOptions \(b, opt) -> modify'
        \dflags -> if b then dflags `DynFlags.gopt_set` opt else dflags `DynFlags.gopt_unset` opt
      forM_ sFlagsExtensions \(b, ext) -> modify'
        \dflags -> if b then dflags `DynFlags.xopt_set` ext else dflags `DynFlags.xopt_unset` ext
      modify' \dflags ->
        dflags { DynFlags.includePaths = DynFlags.addGlobalInclude (DynFlags.includePaths dflags)
                                                                   sFlagsGlobalIncludePaths
               }

    gitlabResourceGetter fn opts =
      "https://gitlab.haskell.org/ghc/ghc/tree/master/" <> Text.pack fn <> case opts of
        Nothing -> ""
        Just s  -> case srcSpanStart s of
          RealSrcLoc l -> "#L" <> Text.pack (show $ srcLocLine l)
          _ -> "#"

defaultAppContext :: MonadIO m => m AppContext
defaultAppContext = appContext $
  AppContextCommon { sFlagsExtensions         = []
                   , sFlagsGeneralOptions     =
                       [(True, DynFlags.Opt_Haddock), (True, DynFlags.Opt_KeepRawTokenStream)]
                   , sFlagsGlobalIncludePaths = [ "dummy_includes"
                                                , "output/ghc/compiler"
                                                , "output/ghc/libraries/base/include"
                                                ]
                   }

newtype AppT (m :: Type -> Type) a = AppT { runAppT :: AppContext -> m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    ) via (ReaderT AppContext m)

deriving
  via (MonadReader (ReaderT AppContext m))
  instance Monad m => HasReader "AppContext" AppContext (AppT m)

deriving
  via (Field "envDynFlags" "ctx" (MonadReader (ReaderT AppContext m)))
  instance Monad m => HasReader "envDynFlags" DynFlags.DynFlags (AppT m)

instance Monad m => HasSourceResourceGetter (AppT m) where
  sourceResourceGetter fn opts = do
    ctx <- ask @"AppContext"
    pure $ envSourceResourceGetter ctx fn opts
