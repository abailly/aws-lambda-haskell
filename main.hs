{-# LANGUAGE FlexibleContexts #-}
module Main where

import           AWS.ApiGateway
import           Control.Lens
import           Control.Monad.Trans.AWS
import           Data.Text               (pack)
import           Prelude                 hiding (null)
import           System.Build
import           System.Docker
import           System.Environment
import           System.IO
import           System.Process          (callProcess)

main :: IO ()
main = do
  [ apiEndpoint, sourceDirectory, targetName ] <- getArgs
  -- build docker container
  buildDocker
  -- build executable with docker
  exe <- stackInDocker (ImageName "ghc-centos") sourceDirectory targetName
  -- pack executable with js shim in .zip file
  _ <- packLambda exe
  lgr <- newLogger Trace stdout
  env <- newEnv Ireland Discover <&> envLogger .~ lgr
  createApiEndpoint env apiEndpoint  >>= print

    where

      createApiEndpoint env api = runResourceT (runAWST env $ createApi $ pack api)

      buildDocker :: IO ()
      buildDocker = callProcess "docker" ["build", "-t", "ghc-centos","ghc-centos" ]

      packLambda :: FilePath -> IO ()
      packLambda exe = callProcess "zip" [ "lambda.zip", "run.js", exe ]
