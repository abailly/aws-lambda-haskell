{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module Main where

import           AWS.ApiGateway
import           AWS.Lambda
import           Control.Lens
import           Control.Monad           (forM)
import           Control.Monad.Trans.AWS
import           Data.Functor            (void)
import           Data.List               (isPrefixOf)
import           Data.Text               (unpack)
import           Prelude                 hiding (null)
import           System.Build
import           System.Config
import           System.Docker
import           System.FilePath
import           System.IO
import           System.IO.Extra
import           System.Process


main :: IO ()
main = options >>= go

initAWS :: IO Env
initAWS = do
  lgr <- newLogger Trace stdout
  awsEnv <- newEnv Ireland Discover <&> envLogger .~ lgr
  return awsEnv

go :: MainConfig -> IO ()
go CreateApi{..} = initAWS >>= \ awsEnv -> runResourceT (runAWST awsEnv $ createApi createApiEndpoint lambdaTargetName) >>= print
go DeleteApi{..} = initAWS >>= \ awsEnv -> runResourceT (runAWST awsEnv $ deleteApi deleteApiEndpoint)
go BuildLambda{..} = do
  -- build docker container
  buildDocker
  -- build executable with docker
  exe <- stackInDocker (ImageName "ghc-centos:lapack") (unpack lambdaSrcDirectory) (unpack lambdaTargetName)
  -- extract supplementary libs...
  libs <- extractLibs (ImageName "ghc-centos:lapack") (unpack lambdaTargetName)
  -- pack executable with js shim in .zip file
  packLambda exe (exe:libs)
    where
      buildDocker :: IO ()
      buildDocker = callProcess "docker" ["build", "-t", "ghc-centos:lapack","ghc-centos" ]

      packLambda :: FilePath -> [FilePath] -> IO ()
      packLambda exe files = do
        runner <- setMainTo exe <$> readFile "run-tmpl.js"
        writeFile "run.js" runner
        callProcess "zip" $ [ "lambda.zip", "run.js" ] ++ files

go DeployLambda{..} = do
  awsEnv <- initAWS
  createOrUpdateFunction awsEnv lambdaTargetName "lambda.zip" >>= print
    where
      createOrUpdateFunction awsEnv target zipFile = runResourceT (runAWST awsEnv $ createFunctionWithZip target zipFile)

setMainTo :: FilePath -> String -> String
setMainTo _   []                             = []
setMainTo exe s |  "$$main$$" `isPrefixOf` s = exe ++ setMainTo exe (drop 8 s)
                |  otherwise                 = head s : setMainTo exe (tail s)


extractLibs :: ImageName -> String -> IO [ FilePath ]
extractLibs (ImageName imgName) targetName = do
  cid <- readFile ".cidfile"
  stackRoot <- filter (/= '\n') <$> readProcess "docker" [ "run", "--rm", "--volumes-from=" ++ cid,  "-w", "/build", imgName, "stack", "path",  "--allow-different-user", "--local-install-root" ] ""
  libs          <- getUnknownLibs <$> readProcess "docker" ["run", "--rm", "--volumes-from=" ++ cid, imgName, "ldd", stackRoot ++ "/bin/" ++ targetName ] ""
  forM libs (extractLib cid)
    where
      extractLib cid lib = do
        let targetLib = takeFileName lib
        (_, Just hout, _, phdl) <- createProcess $ (proc "docker" ["run", "--rm", "--volumes-from=" ++ cid, imgName, "sh", "-c", "dd if=$(readlink -f " ++ lib ++ ")" ]) { std_out = CreatePipe }
        withBinaryFile targetLib WriteMode $ \ hDst -> copy hout hDst
        void $ waitForProcess phdl
        return targetLib


-- | Extract list of non-standard libs to be packaged with executable
--
-- expect input string to be the result of executing `ldd` on some executable
getUnknownLibs :: String -> [ FilePath ]
getUnknownLibs lddOutput = let mappings = map words $ lines lddOutput
                           in map (!! 2) $ filter (not . (`elem` standardLibs) . head ) mappings

-- | List of standard libraries packaged in CentOS image
standardLibs :: [ String ]
standardLibs =  [ "linux-vdso.so.1"
                , "librt.so.1"
                , "libutil.so.1"
                , "libdl.so.2"
                , "libz.so.1"
                , "libgmp.so.10"
                , "libm.so.6"
                , "libpthread.so.0"
                , "libgcc_s.so.1"
                , "libc.so.6"
                , "/lib64/ld-linux-x86-64.so.2"
                ]



