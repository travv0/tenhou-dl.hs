{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

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

downloadReplay :: ByteString -> FilePath -> IO (Maybe Text)
downloadReplay url path = do
  let mfileName = fileNameFromUrl url
  case mfileName of
    Nothing -> do
      putStrLn $ "*** Error getting file name from url: " ++ show url
      return Nothing
    Just fileName -> do
      let fullPath = path </> T.unpack fileName
      needsDownload <- shouldDownload fullPath
      if needsDownload
        then do
          createDirectoryIfMissing True path
          case replay of
            Just r -> do
              putStrLn $ T.unpack (decodeUtf8 url) ++ " ==>\n  " ++ fullPath
              responseBody <$> r >>= BS.writeFile fullPath
              return $ Just $ T.pack fullPath
            Nothing -> do
              putStrLn $ "*** Error parsing url: " ++ show url
              return Nothing
        else return Nothing
 where
  replay = case parseUrlHttps url of
    Just (u, s) -> Just $ runReq def $ req GET u NoReqBody bsResponse s
    Nothing     -> Nothing

downloadReplays :: [ByteString] -> FilePath -> IO [Text]
downloadReplays urls path = catMaybes <$> mapM (`downloadReplay` path) urls

fileNameFromUrl :: ByteString -> Maybe Text
fileNameFromUrl url =
  let [_, queryParams] = T.splitOn "?" $ decodeUtf8 url
      mlogName         = T.stripPrefix "log=" queryParams
  in  flip T.append ".mjlog" <$> mlogName

shouldDownload :: FilePath -> IO Bool
shouldDownload path = not <$> doesFileExist path
