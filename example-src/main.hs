module Main where

import           System.IO

main :: IO ()
main = do
  ln <- getLine
  putStrLn $ "Got: " ++ ln

