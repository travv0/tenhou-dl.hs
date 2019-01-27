module Main where

import           Lib

import qualified Data.Text                     as T
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  if length args < 2
    then putStrLn "Usage: tenhou-dl <Tenhou ID> <Log path>"
    else do
      putStrLn "Downloading new replays..."
      let (tenhouId : path : _) = args
      tags <- getTags (TenhouID $ T.pack tenhouId)
      downloadReplays (parseDownloadUrls tags) path
