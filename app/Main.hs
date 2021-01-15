module Main where

import Lib

import qualified Data.Text as T
import Data.Version (showVersion)
import Paths_tenhou_dl (version)
import System.Environment (getArgs)

main :: IO ()
main = do
  putStrLn $ "tenhou-dl v" ++ showVersion version
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
