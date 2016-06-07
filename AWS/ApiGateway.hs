module AWS.ApiGateway where

import Data.Functor(void)
import  Control.Monad.Catch
import           Control.Lens
import           Control.Monad.Trans.AWS
import           Network.AWS.APIGateway
import Control.Monad.Trans.Resource
import Data.Text(Text)

createApi :: (MonadResource m, MonadCatch m) => Text -> AWST m Method
createApi api = do
    restApi <- send (createRestAPI api)
    let Just apiId = restApi ^. raId
    resources :: [Resource] <- view grrsItems <$> send (getResources apiId)
    let Just rootId = head resources ^. rId
    resrc :: Resource <- send (createResource apiId rootId "{file}")
    let Just fileResourceId = resrc ^. rId
    meth :: Method <- send (putMethod apiId fileResourceId "GET" "NONE")
    return meth

deleteApi :: (MonadResource m, MonadCatch m) => Text -> AWST m ()
deleteApi = void . send . deleteRestAPI
