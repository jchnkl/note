{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Database.HDBC
import qualified Database.HDBC as Db
import qualified Database.HDBC.Sqlite3 as Db

import System.FilePath.Posix ((</>))
-- import Control.Applicative ((<$>), (<|>))
-- import Control.Monad.Trans.Maybe
import Data.Void
import Control.Exception (throw)
import Control.Monad.IO.Class
import Control.Monad.Catch (MonadMask, bracket)
import Data.Time.Clock (getCurrentTime)
import Data.Digest.Pure.SHA (sha256)
import Control.Monad.Except (MonadError(..))
import qualified Data.ByteString.Char8 as BS

import Note.Config
import Note.Types
import Note.TH.Sql

$(mkSqlSchema)
$(mkSqlInsert)
$(mkSqlLookup)

withDbConnection :: (MonadMask m, MonadIO m) => (Db.Connection -> m a) -> m a
withDbConnection = bracket connect (liftIO . Db.disconnect)
    where connect = (</> dbName) <$> dataHome >>= liftIO . Db.connectSqlite3

createNote :: Content -> IO Note
createNote content = note <$> getCurrentTime
    where note time = Note (sha256 content) time content

testNote :: IO Note
testNote = createNote "foo bar baz quux"

eitherToError :: MonadError e m => Either e a -> m a
eitherToError (Left e)  = throwError e
eitherToError (Right a) = return a

-- toNote :: MonadError RowError m => [SqlValue] -> m Note
-- toNote = eitherToError . fromSqlRow

toNote :: [SqlValue] -> Note
toNote v = let (Right note) = fromSqlRow v in note

main :: IO ()
main = withDbConnection $ \db -> do
    -- withTransaction!

    Db.withTransaction db $ \db' -> do

        Db.runRaw db sqlSchema
        -- Db.commit db

        note <- testNote

        Db.run db sqlInsert (toSqlRow note)

    -- Db.run db sqlInsert (toSqlRow note)

    -- Db.commit db

        Db.quickQuery' db sqlLookup [Db.toSql $ _key note]
            >>= putStrLn . show . map toNote

        error "yo man"

        Db.quickQuery' db sqlLookup [Db.toSql $ BS.pack $ "frob"]
            >>= putStrLn . show . map toNote

        -- >>= putStrLn . show . map (fromSqlRow :: [SqlValue] -> Either RowError Note)
    -- Db.run db sqlInsert [Db.toSql "key", Db.toSql "time", Db.toSql "title"]
    return ()

