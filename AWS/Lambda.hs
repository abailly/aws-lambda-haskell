{-# LANGUAGE OverloadedStrings #-}
module AWS.Lambda where

import           Control.Lens
import           Control.Monad.Catch
import           Control.Monad.Trans          (liftIO)
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Resource
import qualified Data.ByteString.Lazy         as LBS
import           Data.Text                    (Text)
import           Network.AWS.Lambda

-- | Default role to use for creating function
--
-- TODO: parameterize
defaultRole :: Text
defaultRole = "arn:aws:iam::259394719635:role/lambda"

defaultHandler :: Text
defaultHandler = "run.handle"

createFunctionWithZip :: (MonadResource m, MonadCatch m) => Text -> FilePath -> AWST m FunctionConfiguration
createFunctionWithZip fName zipFile = do
  code <- liftIO $ LBS.readFile zipFile
  send (createFunction fName NODEJS4_3 defaultRole defaultHandler (set fcZipFile (Just $ LBS.toStrict code) functionCode))
