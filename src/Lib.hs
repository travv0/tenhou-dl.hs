{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import           Control.Monad
import           Data.Default
import           Data.ByteString                ( ByteString )
import qualified Data.ByteString               as BS
import           Data.Maybe
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Text.Encoding
import           Network.HTTP.Req
import           System.FilePath
import           System.Directory
import           Text.HTML.TagSoup

newtype TenhouID = TenhouID {getTenhouID :: Text}

getResponse :: TenhouID -> IO BsResponse
getResponse tenhouId = runReq def $ req
  GET
  (https "tenhou.net" /: "0" /: "log" /: "find.cgi")
  NoReqBody
  bsResponse
  ("un" =: getTenhouID tenhouId)

parseResponseTags :: BsResponse -> [Tag ByteString]
parseResponseTags = parseTags . responseBody

getTags :: TenhouID -> IO [Tag ByteString]
getTags = fmap parseResponseTags . getResponse

parseDownloadUrls :: [Tag ByteString] -> [ByteString]
parseDownloadUrls tags = map aHref downloadLinks
 where
  aSections     = sections (~== ("<a>" :: String)) tags
  downloadLinks = filter ((== "DOWNLOAD") . fromTagText . (!! 1)) aSections
  aHref         = BS.append "https://tenhou.net" . fromAttrib "href" . head

downloadReplay :: Url 'Https -> Option 'Https -> FilePath -> IO ()
downloadReplay url scheme path = do
  fileName <- fileNameFromResponse <$> replay
  let fullPath = path </> fromMaybe "replay.mjlog" fileName
  createDirectoryIfMissing True path
  responseBody <$> replay >>= BS.writeFile fullPath
  putStrLn $ "Downloaded replay to " ++ fullPath
  where replay = runReq def $ req GET url NoReqBody bsResponse scheme

downloadReplays :: [ByteString] -> FilePath -> IO ()
downloadReplays urls path = do
  newUrls <- filterM (shouldDownload path) urls
  mapM_ (\u -> uncurry downloadReplay u path) $ mapMaybe parseUrlHttps newUrls

shouldDownload :: FilePath -> ByteString -> IO Bool
shouldDownload path url =
  let [_, queryParams] = T.splitOn "?" $ decodeUtf8 url
      mlogName         = T.stripPrefix "log=" queryParams
  in  case mlogName of
        Just logName -> fmap not $ doesFileExist $ path </> T.unpack logName ++ ".mjlog"
        Nothing      -> return False

fileNameFromResponse :: BsResponse -> Maybe FilePath
fileNameFromResponse res =
  T.unpack . T.filter (/= '"') . T.drop 21 . decodeUtf8 <$> responseHeader
    res
    "content-disposition"
