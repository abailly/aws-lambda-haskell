{-# LANGUAGE FlexibleContexts #-}
module Main where

import           AWS.ApiGateway
import           Control.Lens
import           Control.Monad.Trans.AWS
import           Data.Text               (pack)
import           Prelude                 hiding (null)
import           System.Build
import           System.Environment
import           System.IO

main :: IO ()
main = do
  [ apiEndpoint, sourceDirectory, targetName ] <- getArgs
  -- build docker container
  buildDocker
  -- build executable with docker
  exe <- buildExecutable sourceDirectory targetName
  -- pack executable with js shim in .zip file
  _ <- packLambda exe
  lgr <- newLogger Trace stdout
  env <- newEnv Ireland Discover <&> envLogger .~ lgr
  createApiEndpoint env apiEndpoint  >>= print

    where

      createApiEndpoint env api = runResourceT (runAWST env $ createApi $ pack api)


