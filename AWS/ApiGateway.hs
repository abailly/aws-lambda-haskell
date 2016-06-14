{-# LANGUAGE ScopedTypeVariables #-}
module AWS.ApiGateway where

import           Control.Lens
import           Control.Monad                (when)
import           Control.Monad.Catch
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Resource
import           Data.Functor                 (void)
import           Data.Maybe
import           Data.Text                    (Text)
import           Network.AWS.APIGateway

createApi :: (MonadResource m, MonadCatch m) => Text -> Text -> AWST m Method
createApi api _  = do
    restApi <- send (createRestAPI api)
    let Just apiId = restApi ^. raId
    resources :: [Resource] <- view grrsItems <$> send (getResources apiId)
    let Just rootId = head resources ^. rId
    resrc :: Resource <- send (createResource apiId rootId "{file}")
    let Just fileResourceId = resrc ^. rId
    meth :: Method <- send (putMethod apiId fileResourceId "GET" "NONE")
    return meth

deleteApi :: (MonadResource m, MonadCatch m) => Text -> AWST m ()
deleteApi apiName = do
  restApis <- (listToMaybe . view grarsItems) <$> send getRestAPIs
  mapM_ deleteApiWithName restApis
    where
      deleteApiWithName api = when (api ^. raName == Just apiName) $ do
        let apiId = api ^. raId
        maybe (return ()) (void . send . deleteRestAPI) apiId
