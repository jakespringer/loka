{-# LANGUAGE OverloadedStrings #-}

module Api.Database where

import Database.SQLite.Simple
import Data.String
import Data.Text
import Data.Maybe
import GHC.Int

import Loka

data GamestateField = GamestateField Int64 Int64 Text Text

instance FromRow GamestateField where
  fromRow = GamestateField <$> field <*> field <*> field <*> field

instance ToRow GamestateField where
  toRow (GamestateField game index actor move) = [SQLInteger game, SQLInteger index, SQLText actor, SQLText move]

initializeLokaDatabase :: IO ()
initializeLokaDatabase = do
  conn <- open "test.db"
  execute_ conn ("CREATE DATABASE loka;")
  execute_ conn (Query $ fromString $
       "CREATE TABLE loka.gamestate ("
    ++   "id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,"
    ++   "game INT NOT NULL,"
    ++   "index INT NOT NULL,"
    ++   "actor VARCHAR(255) NOT NULL,"
    ++   "action VARCHAR(4096) NOT NULL"
    ++ ");")

getGamestate :: Integer -> IO GameState
getGamestate game = do
  conn <- open "test.db"
  fields <- query conn "SELECT (game, index, actor, action) FROM loka.gamestate WHERE game=(?) ORDER BY index" (Only game) :: IO [GamestateField]
  return $ Prelude.map fieldToState fields
    where
      fieldToState (GamestateField _ _ actor action) = (unpack actor, read (unpack action) :: GameMove)


-- todo: remove the part of this function that checks for a valid GameMove
-- made that part of something else (LokaService?)
addAction :: Integer -> Actor -> GameMove -> IO (Maybe GameState)
addAction game actor move = do
  conn <- open "test.db"
  fields <- query conn "SELECT * FROM loka.gamestate WHERE game=(?) ORDER BY index" (Only game) :: IO [GamestateField]
  let fieldToState (GamestateField _ _ actor action) = (unpack actor, read (unpack action) :: GameMove)
  let newState = makeMove actor move $ Prelude.map fieldToState fields
  if isJust newState
    then do
      execute conn (Query $ fromString $
           "INSERT INTO loka.gamestate "
        ++ "(game, index, actor, action) "
        ++ "VALUES ((?), (?), (?), (?));") (GamestateField
          (fromIntegral game)
          (fromIntegral ((Prelude.length newState)) - 1)
          (pack actor)
          (pack (show move)))
    else return ()
  return newState
