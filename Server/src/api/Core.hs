{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Core where

import Control.Lens
import Snap.Core
import Snap.Snaplet
import qualified Data.ByteString.Char8 as B

import Api.Services.LokaService

data Api = Api { _lokaService :: Snaplet LokaService }

makeLenses ''Api

apiRoutes :: [(B.ByteString, Handler b Api ())]
apiRoutes = [("status", method GET respondOk)]

respondOk :: Handler b Api ()
respondOk = do
  modifyResponse . setResponseCode $ 200

apiInit :: SnapletInit b Api
apiInit = makeSnaplet "api" "Core Api" Nothing $ do
  ls <- nestSnaplet "" lokaService lokaServiceInit
  addRoutes apiRoutes
  return $ Api ls
