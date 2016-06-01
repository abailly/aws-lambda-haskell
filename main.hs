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
import           System.Process          (callProcess, proc, readProcess)

main :: IO ()
main = do
  [ apiEndpoint, sourceDirectory, targetName ] <- getArgs
  -- build docker container
  buildDocker
  -- build executable with docker
  exe <- stackInDocker (ImageName "ghc-centos:lapack") sourceDirectory targetName
  -- extract supplementary libs...
  libs <- extractLibs (ImageName "ghc-centos:lapack") targetName
  -- pack executable with js shim in .zip file
  _ <- packLambda exe
  lgr <- newLogger Trace stdout
  env <- newEnv Ireland Discover <&> envLogger .~ lgr
  createApiEndpoint env apiEndpoint  >>= print

    where

      createApiEndpoint env api = runResourceT (runAWST env $ createApi $ pack api)

      buildDocker :: IO ()
      buildDocker = callProcess "docker" ["build", "-t", "ghc-centos:lapack","ghc-centos" ]

      packLambda :: FilePath -> IO ()
      packLambda exe = callProcess "zip" [ "lambda.zip", "run.js", exe ]

extractLibs :: ImageName -> String -> IO FilePath
extractLibs (ImageName imgName) targetName = do
  cid <- readFile ".cidfile"
  stackRoot <- filter (/= '\n') <$> readProcess "docker" [ "run", "--rm", "--volumes-from=" ++ cid,  "-w", "/build", imgName, "stack", "path",  "--allow-different-user", "--local-install-root" ] ""
  libs      <- getUnknownLibs <$> readProcess "docker" ["run", "--rm", "--volumes-from=" ++ cid, imgName, "ldd", stackRoot ++ "/bin/" ++ targetName ] ""
  return targetName

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

testLibs = unlines [ "\tlinux-vdso.so.1 =>  (0x00007fff25bed000)"
           , "\tlibblas.so.3 => /lib64/libblas.so.3 (0x00007fa2f8246000)"
           , "\tliblapack.so.3 => /lib64/liblapack.so.3 (0x00007fa2f79db000)"
           , "\tlibrt.so.1 => /lib64/librt.so.1 (0x00007fa2f77d3000)"
           , "\tlibutil.so.1 => /lib64/libutil.so.1 (0x00007fa2f75d0000)"
           , "\tlibdl.so.2 => /lib64/libdl.so.2 (0x00007fa2f73cb000)"
           , "\tlibz.so.1 => /lib64/libz.so.1 (0x00007fa2f71b5000)"
           , "\tlibgmp.so.10 => /lib64/libgmp.so.10 (0x00007fa2f6f3e000)"
           , "\tlibm.so.6 => /lib64/libm.so.6 (0x00007fa2f6c3b000)"
           , "\tlibpthread.so.0 => /lib64/libpthread.so.0 (0x00007fa2f6a1f000)"
           , "\tlibgcc_s.so.1 => /lib64/libgcc_s.so.1 (0x00007fa2f6809000)"
           , "\tlibc.so.6 => /lib64/libc.so.6 (0x00007fa2f6446000)"
           , "\tlibgfortran.so.3 => /lib64/libgfortran.so.3 (0x00007fa2f6124000)"
           , "\t/lib64/ld-linux-x86-64.so.2 (0x0000558161618000)"
           , "\tlibquadmath.so.0 => /lib64/libquadmath.so.0 (0x00007fa2f5ee7000)"
           ]


