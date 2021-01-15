module Main where

import Lib

import qualified Data.Text as T
import System.Environment

version :: String
version = "1.0.2"

main :: IO ()
main = do
  putStrLn $ "tenhou-dl v" ++ version
  args <- getArgs
  case args of
    [tenhouId, path] -> do
      tags <- getTags (TenhouID $ T.pack tenhouId)
      count <- length <$> downloadReplays (parseDownloadUrls tags) path
      putStrLn $
        "\nDownloaded " ++ show count ++ " replay"
          ++ if count /= 1
            then "s"
            else ""
    _ ->
      putStrLn $
        "Usage: tenhou-dl <Tenhou ID> <Log path>\n"
          ++ "Example: tenhou-dl ID12345678-6fnB8AoP \"C:\\tenhou\\logs\\\""
