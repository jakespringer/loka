{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing
    -fwarn-hi-shadowing -fno-warn-unused-matches #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Jsonify where

------------------------------------------------------------------------------
import Data.List
import Data.Aeson
import Data.ByteString
import GHC.Generics

import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T

import Loka
import Types

------------------------------------------------------------------------------
-- | Represents a single action that is either sent to the client or recieved
-- from the client. It can be converted to JSON using Aeson and the ToJSON
-- instance that it derives using the DeriveGeneric and DeriveAnyClass
-- extensions.
data JsonAction = JsonAction { actor :: String, move :: GameMove }
  deriving (Show, Generic, ToJSON, FromJSON)

------------------------------------------------------------------------------
-- | Represents an entire game state, as sent back to the client. It can be
-- converted to JSON using Aeson and the ToJSON instance that it derives using
-- the DeriveGeneric and DeriveAnyClass extensions.
data JsonGameState = JsonGameState { actions :: [JsonAction],
  static :: CollapsedGameState } deriving (Show, Generic, ToJSON, FromJSON)
