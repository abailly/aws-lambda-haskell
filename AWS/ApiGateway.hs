module AWS.ApiGateway where

import Control.Monad.Catch
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Data.Monoid
import           Network.AWS.Data
import           Network.AWS.APIGateway
import           System.IO
import Control.Monad.Trans.Resource
import Data.Text(Text)

createApi :: (MonadResource m, MonadCatch m) => Text -> AWST m Method
createApi api = do
    restApi <- send (createRestAPI api)
    let Just apiId = restApi ^. raId
    resources :: [Resource] <- (view grrsItems) <$> send (getResources apiId)
    let Just rootId = head resources ^. rId
    resource :: Resource <- send (createResource apiId rootId "{file}")
    let Just fileResourceId = resource ^. rId
    method :: Method <- send (putMethod apiId fileResourceId "GET" "NONE")
    return method
