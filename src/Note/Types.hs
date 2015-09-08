-- {-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Note.Types where

import Data.Binary (encode, decode)
import Data.ByteString.Lazy (ByteString)
import Data.Time.Clock (UTCTime)
import Data.Digest.Pure.SHA (Digest, SHA256State, sha256, showDigest)
import Data.Convertible.Base (Convertible(..), convError)
import Database.HDBC (SqlValue(..), toSql, fromSql)
-- import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL

type Key = Digest SHA256State
type Time = UTCTime
type Content = ByteString

fromDigest :: Digest SHA256State -> ByteString
fromDigest = encode

toDigest :: ByteString -> Digest SHA256State
toDigest = decode

instance Convertible Key SqlValue where
    safeConvert = Right . SqlString . showDigest

instance Convertible SqlValue Key where
    safeConvert (SqlByteString v) = Right . toDigest . BL.pack . BS.unpack $ v
    safeConvert v                 = convError "could not convert sql value to Key" v

data Note = Note
    { _key :: Key
    , _date :: Time
    , _title :: Content
    }
    deriving (Eq, Ord, Show)

type RowError = String

class SqlRow a where
    toSqlRow :: a -> [SqlValue]
    fromSqlRow :: [SqlValue] -> Either RowError a

instance SqlRow Note where
    toSqlRow (Note i d t) = [toSql i, toSql d, toSql t]
    fromSqlRow vs | length vs == 3 = Right $ Note (fromSql $ vs !! 0) (fromSql $ vs !! 1) (fromSql $ vs !! 2)
                  | otherwise      = Left "could not convert sql row to Note"
