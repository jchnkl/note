{-# LANGUAGE TemplateHaskell #-}

import qualified Database.HDBC as Db
import qualified Database.HDBC.Sqlite3 as Db

import System.FilePath.Posix ((</>))
-- import Control.Applicative ((<$>), (<|>))
-- import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad.Catch (MonadMask, bracket)

import Note.Config
import Note.TH.Sql

$(mkSqlSchema)
$(mkSqlInsert)

withDbConnection :: (MonadMask m, MonadIO m) => (Db.Connection -> m a) -> m a
withDbConnection = bracket connect (liftIO . Db.disconnect)
    where connect = (</> dbName) <$> dataHome >>= liftIO . Db.connectSqlite3

main :: IO ()
main = withDbConnection $ \db -> do
    -- withTransaction!
    Db.runRaw db sqlSchema
    Db.commit db
    Db.run db sqlInsert [Db.toSql "key", Db.toSql "time", Db.toSql "title"]
    Db.commit db
    return ()
