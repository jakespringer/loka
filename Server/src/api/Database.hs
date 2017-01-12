{-# LANGUAGE OverloadedStrings #-}

module Api.Database where

------------------------------------------------------------------------------
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Types
import qualified Data.ByteString as B
import Data.String
import Data.Text hiding (map, length, reverse)
import Data.Maybe
import GHC.Int

import Loka
import Types

------------------------------------------------------------------------------
data GamestateField = GamestateField Int64 Int64 Text Text

instance FromRow GamestateField where
  fromRow = GamestateField <$> field <*> field <*> field <*> field

------------------------------------------------------------------------------
-- | Specifies the default parameters for the PostgreSQL connection for this
-- application. This is just for quick prototyping. TODO: Move this to a config
-- file.
postgresqlParameters :: B.ByteString
postgresqlParameters = "host='localhost' port=5432 dbname='loka'"

------------------------------------------------------------------------------
-- | Creates the Loka table on the PostgreSQL database. NOTE: The database must
-- have already been created with the SQL command `CREATE DATABASE loka;`
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

------------------------------------------------------------------------------
-- | Returns the current game state from the PostgreSQL database for a given
-- game id.
getGamestate :: Integer -> IO (GameState)
getGamestate game = do
  conn <- connectPostgreSQL postgresqlParameters
  fields <- query conn "SELECT game, index, actor, action FROM gamestate WHERE game=(?) ORDER BY index DESC;" (Only game) :: IO [GamestateField]
  return $ fieldsGamestate fields

------------------------------------------------------------------------------
-- | Adds an action to the PostgreSQL database.
addAction :: Integer -> Actor -> GameMove -> IO GameState
addAction game actor move = do
  conn <- connectPostgreSQL postgresqlParameters
  fields <- query conn "SELECT game, index, actor, action FROM gamestate WHERE game=(?) ORDER BY index DESC;" (Only game) :: IO [GamestateField]
  execute conn (Query $ fromString $
       "INSERT INTO gamestate \
          \(game, index, actor, action) \
          \VALUES ((?), (?), (?), (?));") (
      (fromIntegral game) :: Int64,
      (fromIntegral (length fields)) :: Int64,
      (pack actor),
      (pack (show move)))
  return ((actor, move) : (fieldsGamestate fields))

------------------------------------------------------------------------------
-- | Utility function to convert a list of GamestateField (as returned by the
-- database) into a game state that can be used by the rest of the code.
fieldsGamestate :: [GamestateField] -> GameState
fieldsGamestate fields = map fieldState fields
  where
    fieldState (GamestateField _ _ actor action) = (unpack actor, read (unpack action) :: GameMove)
