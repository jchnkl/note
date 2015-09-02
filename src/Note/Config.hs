module Note.Config where

import Data.Maybe (fromMaybe)
import Data.List (lookup)
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Control.Applicative ((<$>), (<|>))
import System.Exit (die)
import System.Environment (getEnv, getEnvironment)
import System.FilePath.Posix ((</>))

hoistMaybeT :: Monad m => Maybe a -> MaybeT m a
hoistMaybeT = MaybeT . return

safeGetEnv :: MonadIO m => String -> MaybeT m FilePath
safeGetEnv v = lookup v <$> liftIO getEnvironment >>= hoistMaybeT

xdgDataHome :: MonadIO m => MaybeT m FilePath
xdgDataHome = safeGetEnv "XDG_DATA_HOME"

altDataHome :: MonadIO m => MaybeT m FilePath
altDataHome = (</> ".local/share") <$> safeGetEnv "HOME"

dataHome :: MonadIO m => m FilePath
dataHome = runMaybeT (xdgDataHome <|> altDataHome) >>= maybe exit return
    where exit = liftIO $ die "Please set $XDG_DATA_HOME or $HOME"

dbName :: FilePath
dbName = "notes.db"
