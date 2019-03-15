module Main where

import           Lib

import           System.Environment
import qualified Data.Text                     as T

version :: String
version = "1.0.1"

main :: IO ()
main = do
  putStrLn $ "tenhou-dl v" ++ version
  args <- getArgs
  if length args /= 2
    then
      putStrLn
      $  "Usage: tenhou-dl <Tenhou ID> <Log path>\n"
      ++ "Example: tenhou-dl ID12345678-6fnB8AoP \"C:\\tenhou\\logs\\\""
    else do
      let (tenhouId : path : _) = args
      tags  <- getTags (TenhouID $ T.pack tenhouId)
      count <- length <$> downloadReplays (parseDownloadUrls tags) path
      putStrLn $ "\nDownloaded " ++ show count ++ " replay" ++ if count /= 1
        then "s"
        else ""
