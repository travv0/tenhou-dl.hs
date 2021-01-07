{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Concurrent.Lock (Lock)
import qualified Control.Concurrent.Lock as Lock
import Control.Monad.Catch (catchAll)
import Control.Monad.Except (MonadIO (liftIO), liftEither, runExceptT)
import qualified Control.Monad.Parallel as P
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Default (Default (def))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import GHC.IO.Exception (IOException (ioe_description))
import Network.HTTP.Req (
    BsResponse,
    GET (GET),
    NoReqBody (NoReqBody),
    bsResponse,
    https,
    parseUrlHttps,
    req,
    responseBody,
    runReq,
    (/:),
    (=:),
 )
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO.Error (catchIOError)
import Text.HTML.TagSoup (Tag, fromAttrib, fromTagText, parseTags, sections, (~==))

newtype TenhouID = TenhouID {getTenhouID :: Text}
    deriving (Show)

newtype Url = Url {getUrl :: Text}
    deriving (Show)

getResponse :: TenhouID -> IO BsResponse
getResponse (TenhouID tenhouId) =
    runReq def $
        req
            GET
            (https "tenhou.net" /: "0" /: "log" /: "find.cgi")
            NoReqBody
            bsResponse
            ("un" =: tenhouId)

parseResponseTags :: BsResponse -> [Tag ByteString]
parseResponseTags = parseTags . responseBody

getTags :: TenhouID -> IO [Tag ByteString]
getTags = fmap parseResponseTags . getResponse

parseDownloadUrls :: [Tag ByteString] -> [Url]
parseDownloadUrls tags = map (Url . decodeUtf8 . aHref) downloadLinks
  where
    aSections = sections (~== ("<a>" :: String)) tags
    downloadLinks = filter ((== "DOWNLOAD") . fromTagText . (!! 1)) aSections
    aHref = BS.append "https://tenhou.net" . fromAttrib "href" . head

downloadReplay :: Lock -> Url -> FilePath -> IO (Either String (Maybe FilePath))
downloadReplay lock url path =
    runExceptT
        ( do
            fileName <- liftEither $ fileNameFromUrl url
            let subdir = T.take 6 fileName
            let downloadPath = path </> T.unpack subdir
            let fullPath = downloadPath </> T.unpack fileName
            needsDownload <- liftIO $ shouldDownload fullPath
            if needsDownload
                then do
                    liftIO $ createDirectoryIfMissing True downloadPath
                    replay <- liftIO (getResponseFromUrl url) >>= liftEither
                    liftIO $
                        Lock.with lock $
                            putStrLn $
                                T.unpack (getUrl url)
                                    ++ " ==>\n  "
                                    ++ fullPath
                    liftIO $ BS.writeFile fullPath $ responseBody replay
                    return $ Just fullPath
                else return Nothing
        )
        `catchAll` \e -> return $ Left $ show e

getResponseFromUrl :: Url -> IO (Either String BsResponse)
getResponseFromUrl (Url url) = case parseUrlHttps (encodeUtf8 url) of
    Just (u, s) ->
        sequence $ Right $ runReq def $ req GET u NoReqBody bsResponse s
    Nothing -> return $ Left $ "Error parsing url: " ++ show url

downloadReplays :: [Url] -> FilePath -> IO [FilePath]
downloadReplays urls path = do
    lock <- Lock.new
    catMaybes
        <$> P.mapM (\u -> downloadReplay lock u path >>= unwrapOrPrintError lock) urls

unwrapOrPrintError :: Monoid a => Lock -> Either String a -> IO a
unwrapOrPrintError lock (Left e) = Lock.with lock $ putStrLn ("*** " ++ e) >> return mempty
unwrapOrPrintError _ (Right p) = return p

maybeToEither :: b -> Maybe a -> Either b a
maybeToEither = flip maybe Right . Left

fileNameFromUrl :: Url -> Either String Text
fileNameFromUrl (Url url) = do
    let splitUrl = T.splitOn "?" url
    queryParams <- case splitUrl of
        (_ : ps : _) -> Right ps
        _ -> Left $ "Error getting query parameters from url: " ++ show url
    logName <-
        maybeToEither
            ( "Error getting log name from query parameters: "
                ++ T.unpack queryParams
            )
            $ T.stripPrefix "log=" queryParams
    return $ T.append logName ".mjlog"

shouldDownload :: FilePath -> IO Bool
shouldDownload path = not <$> doesFileExist path
