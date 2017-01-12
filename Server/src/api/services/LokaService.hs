{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

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
import Api.Database
import Jsonify

------------------------------------------------------------------------------
data LokaService = LokaService

makeLenses ''LokaService

------------------------------------------------------------------------------
lokaRoutes :: [(B.ByteString, Handler b LokaService ())]
lokaRoutes = [("/game/:gameName/state", method GET gameStateHandler),
              ("/game/:gameName/action", method POST gameActionHandler)]

------------------------------------------------------------------------------
-- | Handles and responds to a request to read the game state. This will read
-- the state from the PostgreSQL database, format it to JSON, and send it back
-- to the client.
gameStateHandler :: Handler b LokaService ()
gameStateHandler = do
  gameName <- getParam "gameName"
  case gameName of
    Nothing -> do
      writeBS "Invalid gameName"
      return ()
    Just name -> do
      gamestate <- liftIO . getGamestate $ ((read $ B.unpack name) :: Integer)
      case collapseGameState (Just gamestate) of
        Nothing -> do
          writeBS "Invalid gameName"
          return ()
        Just staticState -> do
          writeBS $ LB.toStrict $ encode $ (JsonGameState (map jsonGameState gamestate) staticState)
          return ()
            where
              jsonGameState (actor, move) = JsonAction actor move

------------------------------------------------------------------------------
-- | Handles and responds to a request to make an action. This takes in the
-- current game state from the PostgreSQL database, adds an action if the
-- action is valid, writes the new action the database, and then sends back
-- the updated state.
gameActionHandler :: Handler b LokaService ()
gameActionHandler = do
  -- todo: rewrite this function to be pretty (use Maybe monad)
  gameName <- getParam "gameName"
  case gameName of
    Nothing -> do
      writeBS "Invalid gameName"
      return ()
    Just name -> do
      let gameId = ((read $ B.unpack name) :: Integer)
      gamestate <- liftIO . getGamestate $ gameId
      body <- readRequestBody 16384
      case decode body of
        Nothing -> do
          writeBS "Invalid gameName"
          return ()
        Just (JsonAction actor move) -> do
          case appendGameMove actor move (Just gamestate) of
            Nothing -> do
              writeBS "Invalid gameName"
              return ()
            Just newState -> do
              liftIO $ addAction gameId actor move
              case collapseGameState (Just newState) of
                Nothing -> do
                  writeBS "Invalid gameName"
                  return ()
                Just staticState -> do
                  writeBS $ LB.toStrict $ encode $ (JsonGameState (map jsonGameState newState) staticState)
                  return ()
                    where
                      jsonGameState (actor, move) = JsonAction actor move

  return ()

------------------------------------------------------------------------------
-- | Called by Snap to create the Loka service snaplet
lokaServiceInit :: SnapletInit b LokaService
lokaServiceInit = makeSnaplet "loka" "Loka Service" Nothing $ do
  addRoutes lokaRoutes
  return $ LokaService
