{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Lib where

import           Prelude                 hiding ( writeFile )

import           Control.Monad.Except
import           Data.ByteString                ( ByteString )
import           Data.Default
import           Data.Maybe
import           Data.Text                      ( Text )
import           Data.Text.Encoding
import           Network.HTTP.Req        hiding ( Url )
import qualified System.Directory              as D
import           System.FilePath
import           Text.HTML.TagSoup
import qualified Data.ByteString               as BS
import qualified Data.Text                     as T

newtype TenhouID = TenhouID {getTenhouID :: Text}
  deriving Show

newtype Url = Url {getUrl :: Text}
  deriving Show

class Monad m => MonadRequest m where
  runRequest :: HttpConfig -> Req a -> m a

instance MonadRequest IO where
  runRequest = runReq

class Monad m => MonadFS m where
  doesFileExist :: FilePath -> m Bool
  createDirectoryIfMissing :: Bool -> FilePath -> m ()
  writeFile :: FilePath -> ByteString -> m ()

instance MonadFS IO where
  doesFileExist = D.doesFileExist
  createDirectoryIfMissing = D.createDirectoryIfMissing
  writeFile = BS.writeFile

getResponse :: MonadRequest m => TenhouID -> m BsResponse
getResponse tenhouId = runRequest def $ req
  GET
  (https "tenhou.net" /: "0" /: "log" /: "find.cgi")
  NoReqBody
  bsResponse
  ("un" =: getTenhouID tenhouId)

parseResponseTags :: BsResponse -> [Tag ByteString]
parseResponseTags = parseTags . responseBody

getTags :: MonadRequest m => TenhouID -> m [Tag ByteString]
getTags = fmap parseResponseTags . getResponse

parseDownloadUrls :: [Tag ByteString] -> [Url]
parseDownloadUrls tags = map (Url . decodeUtf8 . aHref) downloadLinks
 where
  aSections     = sections (~== ("<a>" :: String)) tags
  downloadLinks = filter ((== "DOWNLOAD") . fromTagText . (!! 1)) aSections
  aHref         = BS.append "https://tenhou.net" . fromAttrib "href" . head

downloadReplay
  :: (MonadFS m, MonadRequest m)
  => Url
  -> FilePath
  -> m (Either String (Maybe FilePath))
downloadReplay url path = runExceptT $ do
  fileName <- liftEither $ fileNameFromUrl url
  let subdir       = T.take 6 fileName
  let downloadPath = path </> T.unpack subdir
  let fullPath     = downloadPath </> T.unpack fileName
  needsDownload <- lift $ shouldDownload fullPath
  if needsDownload
    then do
      lift $ createDirectoryIfMissing True downloadPath
      response <- lift $ getResponseFromUrl url
      replay   <- liftEither response
      lift $ writeFile fullPath $ responseBody replay
      return $ Just fullPath
    else return Nothing

getResponseFromUrl :: MonadRequest m => Url -> m (Either String BsResponse)
getResponseFromUrl url = case parseUrlHttps (encodeUtf8 $ getUrl url) of
  Just (u, s) ->
    sequence . Right $ runRequest def $ req GET u NoReqBody bsResponse s
  Nothing -> return . Left $ "Error parsing url: " ++ show (getUrl url)

downloadReplays
  :: (MonadIO m, MonadFS m, MonadRequest m) => [Url] -> FilePath -> m [FilePath]
downloadReplays urls path =
  catMaybes
    <$> mapM
          (\u -> do
            epath <- downloadReplay u path
            mpath <- unwrapOrPrintError epath
            mapM_ (printCopyString u) mpath
            return mpath
          )
          urls

unwrapOrPrintError
  :: MonadIO m => Either String (Maybe FilePath) -> m (Maybe FilePath)
unwrapOrPrintError (Left  e) = liftIO $ putStrLn ("*** " ++ e) >> return Nothing
unwrapOrPrintError (Right p) = return p

printCopyString :: MonadIO m => Url -> FilePath -> m ()
printCopyString url path =
  liftIO $ putStrLn $ T.unpack (getUrl url) ++ " ==>\n  " ++ path

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

shouldDownload :: MonadFS m => FilePath -> m Bool
shouldDownload path = not <$> doesFileExist path
