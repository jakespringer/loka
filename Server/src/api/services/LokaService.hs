{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

-- Currently this file is buggy and poorly written--it is being used for
-- testing and prototyping. It will be rewritten.
module Api.Services.LokaService where

------------------------------------------------------------------------------
import Data.Aeson
import Data.ByteString hiding (map, length)
import Control.Lens
import Control.Monad.IO.Class
import Snap.Core
import Snap.Snaplet
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T

import Loka
import Types
import Api.Database
import Jsonify

------------------------------------------------------------------------------
data LokaService = LokaService

makeLenses ''LokaService

------------------------------------------------------------------------------
lokaRoutes :: [(B.ByteString, Handler b LokaService ())]
lokaRoutes = [("/game/:gameId/state", method GET gameStateHandler),
              ("/game/:gameId/allmoves", method GET gameAllMovesHandler),
              ("/game/:gameId/doaction", method POST gameDoActionHandler)]

------------------------------------------------------------------------------
-- | Handles and responds to a request to read the game state. This will read
-- the state from the PostgreSQL database, format it to JSON, and send it back
-- to the client.
gameStateHandler :: Handler b LokaService ()
gameStateHandler = do
  modifyResponse $ setHeader "Access-Control-Allow-Origin" "*"
  modifyResponse $ setHeader "Content-Type" "application/json"
  gameId <- getParam "gameId"
  maybe (writeBS $ jsonError "Invalid game ID") (\gameIdValid -> do
    gameState <- liftIO . getGameState $ ((read $ B.unpack gameIdValid) :: Integer)
    writeBS $ LB.toStrict $ encode $ JsonGameState (map (uncurry JsonAction) gameState)
      $ collapseGameState gameState)
    gameId

------------------------------------------------------------------------------
-- | Handles and responds to a request to make an action. This takes in the
-- current game state from the PostgreSQL database, adds an action if the
-- action is valid, writes the new action the database, and then sends back
-- the updated state.
gameDoActionHandler :: Handler b LokaService ()
gameDoActionHandler = do
  modifyResponse $ setHeader "Access-Control-Allow-Origin" "*"
  modifyResponse $ setHeader "Content-Type" "application/json"
  gameId <- getParam "gameId"
  maybe (writeBS $ jsonError "Invalid game ID") (\gameIdValid -> do
    gameState <- liftIO . getGameState $ ((read $ B.unpack gameIdValid) :: Integer)
    body <- readRequestBody 16384
    maybe (writeBS $ jsonError "Unable to parse JSON") (\action -> do
      maybe (writeBS $ jsonError "Invalid game move") (\newGameState -> do
        liftIO $ addAction ((read $ B.unpack gameIdValid) :: Integer)
          (actor action)
          (Jsonify.move action)
        writeBS $ LB.toStrict $ encode $ JsonGameState (map (uncurry JsonAction) newGameState)
          $ collapseGameState newGameState)
            $ appendGameMove (actor action) (Jsonify.move action) gameState)
                $ decode body)
                  gameId

------------------------------------------------------------------------------
-- | Handles and responds to a request to get all possible moves for a given
-- color.
gameAllMovesHandler :: Handler b LokaService ()
gameAllMovesHandler = do
  modifyResponse $ setHeader "Access-Control-Allow-Origin" "*"
  modifyResponse $ setHeader "Content-Type" "application/json"
  gameId <- getParam "gameId"
  colorParam <- getQueryParam "color"
  maybe (writeBS $ jsonError "Invalid game ID") (\gameIdValid -> do
    maybe (writeBS $ jsonError "Invalid color") (\colorValid -> do
      gameState <- liftIO . getGameState $ ((read $ B.unpack gameIdValid) :: Integer)
      writeBS $ LB.toStrict $ encode $ allPossibleMoves
        (collapseGameState gameState)
        (read (B.unpack colorValid) :: PieceColor)) colorParam) gameId

------------------------------------------------------------------------------
-- | Called by Snap to create the Loka service snaplet
lokaServiceInit :: SnapletInit b LokaService
lokaServiceInit = makeSnaplet "loka" "Loka Service" Nothing $ do
  addRoutes lokaRoutes
  return $ LokaService
