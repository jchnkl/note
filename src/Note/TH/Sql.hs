{-# LANGUAGE TemplateHaskell #-}

module Note.TH.Sql where

import Control.Monad.IO.Class
import Language.Haskell.TH

mkSqlSchema :: Q [Dec]
mkSqlSchema = do
    schema <- runIO $ readFile "src/sql/schema.sql"
    [d|sqlSchema = schema|]

mkSqlInsert :: Q [Dec]
mkSqlInsert = do
    insert <- runIO $ readFile "src/sql/insert.sql"
    [d|sqlInsert = insert|]
