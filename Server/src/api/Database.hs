module Api.Database where

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Control.Monad.Reader
import Control.Applicative

import Loka

data GamestateField = GamestateField Integer Integer Integer String String

instance FromRow GamestateField where
  fromRow = GamestateField <$> field <*> field <*> field <*> field <*> field

initializeLokaDatabase :: IO ()
initializeLokaDatabase = do
  conn <- open "test.db"
  execute_ conn ("CREATE DATABASE loka;")
  execute_ conn ("CREATE TABLE loka.gamestate (id INT NOT NULL AUTO_INCREMENT PRIMARY KEY,game INT NOT NULL,index INT NOT NULL,actor VARCHAR(255) NOT NULL,action VARCHAR(4096) NOT NULL);")

getOrderedGamestate :: Integer -> IO GameState
getOrderedGamestate game = do
  conn <- open "test.db"
  fields <- query conn "SELECT * FROM loka.gamestate WHERE game=(?) ORDER BY index" (Only game) :: IO [GamestateField]
  return $ map fieldToState fields
    where
      fieldToState (GamestateField _ _ _ actor action) = (actor, read action :: GameMove)
