module System.Config(MainConfig(..), options) where

import           Data.Text
import           Options.Applicative

data MainConfig = DeleteApi { deleteApiEndpoint :: Text}
                | CreateApi { createApiEndpoint  :: Text
                            , lambdaSrcDirectory :: Text
                            , lambdaTargetName   :: Text
                            }
                deriving (Eq,Show,Read)

mainConfig :: Parser MainConfig
mainConfig = subparser
  ( command "create" (info createConfig
                      (progDesc "create an AWS Lambda Function and ties it to an API Gateway"))
    <> command "delete" (info deleteConfig
                         (progDesc "deleta an AWS Lambda function and associated API Gateway endpoint"))
  )

createConfig :: Parser MainConfig
createConfig = CreateApi
  <$> (pack <$> strOption ( long "endpoint"
                           <> short 'e'
                           <> help "Endpoint identifier, a simple string"))
  <*> (pack <$> strOption ( long "source-directory"
                           <> short 'd'
                           <> value "."
                           <> metavar "FILE"
                           <> help "Source directory of lambda function to deploy"))
  <*> (pack <$> strOption ( long "build-target"
                           <> short 't'
                           <> metavar "STRING"
                           <> help "Target of executable to build and deploy"))


deleteConfig :: Parser MainConfig
deleteConfig = DeleteApi
  <$> (pack <$> strOption ( long "endpoint"
                           <> short 'd'
                           <> help "Endpoint identifier, a simple string. If multiple identifiers match, the first one will be deleted."))

options :: IO MainConfig
options = execParser opts
  where
    opts = info (helper <*> mainConfig)
      ( fullDesc
        <> progDesc "Manage AWS Lambda functions"
        <> header "Haskell + Lambda" )

