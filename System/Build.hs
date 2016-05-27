module System.Build where

import           Data.ByteString  (hGet, hPut, null)
import           Data.Functor     (void)
import           Prelude          hiding (null)
import           System.Directory
import           System.Exit
import           System.IO
import           System.Process

buildDocker :: IO ()
buildDocker = callProcess "docker" ["build", "-t", "ghc-centos","ghc-centos" ]

copy :: Handle -> Handle -> IO ()
copy hIn hOut = do
  bs <- hGet hIn 4096
  if not (null bs)
    then hPut hOut bs >> copy hIn hOut
    else return ()

buildExecutable :: FilePath -> String -> IO FilePath
buildExecutable srcDir targetName = do
  absSrcDir <- canonicalizePath srcDir
  removeFile ".cidfile"
  (_,_,_,hdl) <- createProcess $ proc "docker" ["run", "--cidfile=.cidfile", "-v", absSrcDir ++ ":/build", "-w", "/build" , "ghc-centos","stack", "build","--allow-different-user", targetName ]
  exitCode <- waitForProcess hdl
  case exitCode of
    ExitSuccess -> do
      cid <- readFile ".cidfile"
      (_, Just hout, _, phdl) <- createProcess $ (proc "docker" ["run", "--volumes-from=" ++ cid, "busybox","dd", "if=/build/.stack-work/install/x86_64-linux/lts-5.17/7.10.3/bin/"++ targetName ]) { std_out = CreatePipe }
      withBinaryFile "lambda" WriteMode $ \ hDst -> copy hout hDst
      void $ waitForProcess phdl
      return "lambda"
    ExitFailure code -> fail $ "failed to build correctly " ++ targetName ++ " in directory " ++ srcDir ++ ": " ++ show code

packLambda :: FilePath -> IO ()
packLambda exe = callProcess "zip" [ "lambda.zip", "run.js", exe ]
