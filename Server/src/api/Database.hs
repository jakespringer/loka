{-# LANGUAGE OverloadedStrings #-}

module Api.Database where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import qualified Data.ByteString as B
import Data.String
import Data.Text hiding (map, length)
import Data.Maybe
import GHC.Int

import Loka
import Types

data GamestateField = GamestateField Int64 Int64 Text Text

instance FromRow GamestateField where
  fromRow = GamestateField <$> field <*> field <*> field <*> field

postgresqlParameters :: B.ByteString
postgresqlParameters = "host='localhost' port=5432 dbname='loka'"

initializeLokaDatabase :: IO ()
initializeLokaDatabase = do
  conn <- connectPostgreSQL postgresqlParameters
  -- execute_ conn ("CREATE DATABASE loka;")
  execute_ conn (Query $ fromString $
    "CREATE TABLE gamestate (\
      \id SERIAL,\
      \game INT NOT NULL,\
      \index INT NOT NULL,\
      \actor VARCHAR(255) NOT NULL,\
      \action VARCHAR(4096) NOT NULL\
    \);")
  return ()

getGamestate :: Integer -> IO (GameState)
getGamestate game = do
  conn <- connectPostgreSQL postgresqlParameters
  fields <- query conn "SELECT game, index, actor, action FROM gamestate WHERE game=(?) ORDER BY index;" (Only game) :: IO [GamestateField]
  return $ fieldsGamestate fields

addAction :: Integer -> Actor -> GameMove -> IO GameState
addAction game actor move = do
  conn <- connectPostgreSQL postgresqlParameters
  fields <- query conn "SELECT game, index, actor, action FROM gamestate WHERE game=(?) ORDER BY index;" (Only game) :: IO [GamestateField]
  execute conn (Query $ fromString $
       "INSERT INTO gamestate \
          \(game, index, actor, action) \
          \VALUES ((?), (?), (?), (?));") (
      (fromIntegral game) :: Int64,
      (fromIntegral (length fields)) :: Int64,
      (pack actor),
      (pack (show move)))
  return ((actor, move) : (fieldsGamestate fields))

fieldsGamestate :: [GamestateField] -> GameState
fieldsGamestate fields = map fieldState fields
  where
    fieldState (GamestateField _ _ actor action) = (unpack actor, read (unpack action) :: GameMove)
