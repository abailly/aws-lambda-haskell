module System.Config(MainConfig(..), options) where

import           Data.Text
import           Options.Applicative

data MainConfig = DeleteApi { deleteApiEndpoint :: Text}
                | CreateApi { createApiEndpoint :: Text
                            , lambdaTargetName  :: Text
                            }
                | BuildLambda { lambdaTargetName   :: Text
                              , lambdaSrcDirectory :: Text
                              }
                | DeployLambda { lambdaTargetName :: Text }
                deriving (Eq,Show,Read)

mainConfig :: Parser MainConfig
mainConfig = subparser (
  command "api" (info apiConfig
                 (progDesc "Manipulate AWS API Gateway Endpoints and their relations with Lambda functions"))
  <> command "lambda" (info lambdaConfig
                       (progDesc "Manipulate AWS Lambda functions"))
  )

apiConfig :: Parser MainConfig
apiConfig = subparser (
  command "create" (info createApiConfig
                    (progDesc "Create an AWS API Gateway endpoint and ties it to AWS Lambda function"))
  <> command "delete" (info deleteApiConfig
                       (progDesc "Delete an API Gateway endpoint"))
  )

createApiConfig :: Parser MainConfig
createApiConfig = CreateApi
  <$> (pack <$> strOption ( long "endpoint"
                            <> short 'e'
                            <> help "Endpoint identifier, a simple string"))
  <*> (pack <$> strOption ( long "build-target"
                            <> short 't'
                            <> metavar "STRING"
                            <> help "Target of executable to build and deploy"))

deleteApiConfig :: Parser MainConfig
deleteApiConfig = DeleteApi
  <$> (pack <$> strOption ( long "endpoint"
                           <> short 'e'
                           <> help "Endpoint identifier, a simple string. If multiple identifiers match, the first one will be deleted."))

lambdaConfig :: Parser MainConfig
lambdaConfig = subparser (
  command "deploy" (info deployLambdaConfig
                      (progDesc "Deploy an AWS Lambda function package"))
  <>  command "build" (info buildLambdaConfig
                       (progDesc "Build an AWS Lambda function package using docker"))
  )

buildLambdaConfig :: Parser MainConfig
buildLambdaConfig = BuildLambda
  <$> (pack <$> strOption ( long "build-target"
                            <> short 't'
                            <> metavar "STRING"
                            <> help "Target of executable to build"))
  <*> (pack <$> strOption ( long "source-directory"
                            <> short 'd'
                            <> value "."
                            <> metavar "FILE"
                            <> help "Source directory of lambda function to build"))

deployLambdaConfig :: Parser MainConfig
deployLambdaConfig = DeployLambda
  <$> (pack <$> strOption ( long "build-target"
                            <> short 't'
                            <> metavar "STRING"
                            <> help "Target of executable to deploy"))

options :: IO MainConfig
options = execParser opts
  where
    opts = info (helper <*> mainConfig)
      ( fullDesc
        <> progDesc "Manage AWS Lambda functions"
        <> header "Haskell + Lambda" )

