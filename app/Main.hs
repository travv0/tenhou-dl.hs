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
        [tenhouId, path] -> app (TenhouID $ T.pack tenhouId) path
        _ ->
            putStrLn $
                "Usage: tenhou-dl <Tenhou ID> <Log path>\n"
                    ++ "Example: tenhou-dl ID12345678-6fnB8AoP \"C:\\tenhou\\logs\\\""
