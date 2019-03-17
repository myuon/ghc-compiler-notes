{-# LANGUAGE DerivingVia #-}

module GHC.Compiler.Notes.App where

import           Control.Monad.Catch
import           Control.Monad.Trans.Reader
import           Control.Monad.IO.Class
import           Capability.Accessors
import           Capability.Reader
import           GHC.Generics
import qualified DynFlags
import qualified GHC.Paths
import           GHC (runGhc, getSessionDynFlags)
import           GHC.LanguageExtensions (Extension)
import           Control.Monad
import           Control.Monad.Trans.State
import qualified Packages


data AppContext = AppContext
  { envDynFlags :: DynFlags.DynFlags
  }
  deriving Generic

type OnOff a = (Bool, a)

data AppContextCommon = AppContextCommon
  { sFlagsExtensions :: [OnOff Extension]
  , sFlagsGeneralOptions :: [OnOff DynFlags.GeneralFlag]
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

  return $ AppContext
    { envDynFlags = dflags
    }
  where
    dflagsUpdater = do
      forM_ sFlagsGeneralOptions \(b, opt) ->
        modify' \dflags -> if b
          then dflags `DynFlags.gopt_set` opt
          else dflags `DynFlags.gopt_unset` opt
      forM_ sFlagsExtensions \(b, ext) ->
        modify' \dflags -> if b
          then dflags `DynFlags.xopt_set` ext
          else dflags `DynFlags.xopt_unset` ext
      modify' \dflags -> dflags
        { DynFlags.includePaths
          = DynFlags.addGlobalInclude (DynFlags.includePaths dflags)
            sFlagsGlobalIncludePaths
        }

defaultAppContext :: MonadIO m => m AppContext
defaultAppContext = appContext $ AppContextCommon
  { sFlagsExtensions = []
  , sFlagsGeneralOptions =
    [ (True, DynFlags.Opt_Haddock)
    , (True, DynFlags.Opt_KeepRawTokenStream)
    ]
  , sFlagsGlobalIncludePaths =
    [ "dummy_includes"
    , "output/ghc/compiler"
    , "output/ghc/libraries/base/include"
    ]
  }


newtype AppT m a = AppT
  { runAppT :: AppContext -> m a
  }

deriving via (ReaderT AppContext m) instance Functor m => Functor (AppT m)
deriving via (ReaderT AppContext m) instance Applicative m => Applicative (AppT m)
deriving via (ReaderT AppContext m) instance Monad m => Monad (AppT m)
deriving via (ReaderT AppContext m) instance MonadIO m => MonadIO (AppT m)
deriving via (ReaderT AppContext m) instance MonadThrow m => MonadThrow (AppT m)

deriving
  via (Field "envDynFlags" "ctx" (MonadReader (ReaderT AppContext m)))
  instance Monad m => HasReader "envDynFlags" DynFlags.DynFlags (AppT m)
