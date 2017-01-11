{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Services.LokaService where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Applicative
import Snap.Core
import Snap.Snaplet
import qualified Data.ByteString.Char8 as B

import Api.Types
import Api.Database

data LokaService = LokaService

makeLenses ''LokaService

lokaRoutes :: [(B.ByteString, Handler b LokaService ())]
lokaRoutes = [("/game/:gameName/state", method GET gameStateHandler)]

gameStateHandler :: Handler b LokaService ()
gameStateHandler = do
  gameName <- getParam "gameName"
  maybe (writeBS "You must supply a game name")
         writeBS gameName
  return ()

lokaServiceInit :: SnapletInit b LokaService
lokaServiceInit = makeSnaplet "loka" "Loka Service" Nothing $ do
  addRoutes lokaRoutes
  return $ LokaService
