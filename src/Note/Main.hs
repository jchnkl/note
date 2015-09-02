{-# LANGUAGE TemplateHaskell #-}

import qualified Database.HDBC as Db
import qualified Database.HDBC.Sqlite3 as Db

import System.FilePath.Posix ((</>))
import Control.Monad.IO.Class
import Control.Monad.Catch (MonadMask, bracket)

import Note.Config
import Note.TH.Sql

$(mkSqlSchema)

withDbConnection :: (MonadMask m, MonadIO m) => (Db.Connection -> m a) -> m a
withDbConnection = bracket connect (liftIO . Db.disconnect)
    where connect = (</> dbName) <$> dataHome >>= liftIO . Db.connectSqlite3

main :: IO ()
main = withDbConnection $ \db -> do
    Db.runRaw db sqlSchema
    Db.commit db
    return ()
