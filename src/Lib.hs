{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import           Control.Monad.Except
import           Data.ByteString                ( ByteString )
import           Data.Default
import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.Text.Encoding
import           Network.HTTP.Req        hiding ( Url )
import           System.Directory
import           System.FilePath
import           Text.HTML.TagSoup
import qualified Data.ByteString               as BS
import qualified Data.Text                     as T

newtype TenhouID = TenhouID {getTenhouID :: Text}
  deriving Show

newtype Url = Url {getUrl :: Text}
  deriving Show

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

parseDownloadUrls :: [Tag ByteString] -> [Url]
parseDownloadUrls tags = map (Url . decodeUtf8 . aHref) downloadLinks
 where
  aSections     = sections (~== ("<a>" :: String)) tags
  downloadLinks = filter ((== "DOWNLOAD") . fromTagText . (!! 1)) aSections
  aHref         = BS.append "https://tenhou.net" . fromAttrib "href" . head

downloadReplay :: Url -> FilePath -> IO (Either String (Maybe FilePath))
downloadReplay url path = runExceptT $ do
  fileName <- liftEither $ fileNameFromUrl url
  let subdir       = T.take 6 fileName
  let downloadPath = path </> T.unpack subdir
  let fullPath     = downloadPath </> T.unpack fileName
  needsDownload <- liftIO $ shouldDownload fullPath
  if needsDownload
    then do
      liftIO $ createDirectoryIfMissing True downloadPath
      replay <- liftEither $ getResponseFromUrl url
      liftIO $ putStrLn $ T.unpack (getUrl url) ++ " ==>\n  " ++ fullPath
      liftIO $ responseBody <$> replay >>= BS.writeFile fullPath
      return $ Just fullPath
    else return Nothing

getResponseFromUrl :: Url -> Either String (IO BsResponse)
getResponseFromUrl url = case parseUrlHttps (encodeUtf8 $ getUrl url) of
  Just (u, s) -> Right $ runReq def $ req GET u NoReqBody bsResponse s
  Nothing     -> Left $ "Error parsing url: " ++ show (getUrl url)

downloadReplays :: [Url] -> FilePath -> IO [FilePath]
downloadReplays urls path =
  catMaybes <$> mapM (\u -> downloadReplay u path >>= unwrapOrPrintError) urls

unwrapOrPrintError :: Either String (Maybe FilePath) -> IO (Maybe FilePath)
unwrapOrPrintError (Left  e) = putStrLn ("*** " ++ e) >> return Nothing
unwrapOrPrintError (Right p) = return p

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither = flip maybe Right . Left

fileNameFromUrl :: Url -> Either String Text
fileNameFromUrl url = do
  let splitUrl = T.splitOn "?" $ getUrl url
  queryParams <- case splitUrl of
    (_ : ps : _) -> Right ps
    _ ->
      Left $ "Error getting query parameters from url: " ++ show (getUrl url)
  logName <-
    maybeToEither
        (  "Error getting log name from query parameters: "
        ++ T.unpack queryParams
        )
      $ T.stripPrefix "log=" queryParams
  return $ T.append logName ".mjlog"

shouldDownload :: FilePath -> IO Bool
shouldDownload path = not <$> doesFileExist path
