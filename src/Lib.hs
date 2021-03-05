{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Concurrent.Lock (Lock)
import qualified Control.Concurrent.Lock as Lock
import Control.Lens (view, (.~), (^.))
import Control.Monad ((>=>))
import Control.Monad.Catch (catchAll)
import qualified Control.Monad.Parallel as P
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS
import Data.Function ((&))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import Network.Wreq (Response, get, responseBody)
import qualified Network.Wreq as W
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Text.HTML.TagSoup (
    Tag,
    fromAttrib,
    fromTagText,
    parseTags,
    sections,
    (~==),
 )

newtype TenhouID = TenhouID {getTenhouID :: Text}
    deriving (Show)

newtype Url = Url {getUrl :: Text}
    deriving (Show)

app :: TenhouID -> FilePath -> IO ()
app tenhouId path = do
    tags <- getTags tenhouId
    count <- length <$> downloadReplays (parseDownloadUrls tags) path
    putStrLn $
        "\nDownloaded " ++ show count ++ " replay"
            ++ if count /= 1 then "s" else ""

getResponse :: TenhouID -> IO (Response ByteString)
getResponse (TenhouID tenhouId) =
    let options = W.defaults & W.param "un" .~ [tenhouId]
     in W.getWith options "https://tenhou.net/0/log/find.cgi"

parseResponseTags :: Response ByteString -> [Tag ByteString]
parseResponseTags = parseTags . view responseBody

getTags :: TenhouID -> IO [Tag ByteString]
getTags = fmap parseResponseTags . getResponse

parseDownloadUrls :: [Tag ByteString] -> [Url]
parseDownloadUrls tags =
    map
        ( Url
            . decodeUtf8
            . BS.toStrict
            . aHref
        )
        downloadLinks
  where
    aSections = sections (~== ("<a>" :: String)) tags
    downloadLinks = filter ((== "DOWNLOAD") . fromTagText . (!! 1)) aSections
    aHref = BS.append "https://tenhou.net" . fromAttrib "href" . head

getResponseFromUrl :: Url -> IO (Response ByteString)
getResponseFromUrl (Url url) = get $ T.unpack url

downloadReplays :: [Url] -> FilePath -> IO [FilePath]
downloadReplays urls path = do
    lock <- Lock.new
    catMaybes
        <$> P.mapM
            (downloadReplay lock path >=> unwrapOrPrintError lock)
            urls

downloadReplay :: Lock -> FilePath -> Url -> IO (Either String (Maybe FilePath))
downloadReplay lock path url = do
    case fileNameFromUrl url of
        Left e -> return $ Left e
        Right fileName -> do
            let subdir = T.take 6 fileName
            let downloadPath = path </> T.unpack subdir
            let fullPath = downloadPath </> T.unpack fileName
            needsDownload <- shouldDownload fullPath
            if needsDownload
                then do
                    createDirectoryIfMissing True downloadPath
                    replay <- getResponseFromUrl url
                    Lock.with lock $
                        putStrLn $
                            T.unpack (getUrl url)
                                ++ " ==>\n  "
                                ++ fullPath
                    BS.writeFile fullPath $ replay ^. responseBody
                    return $ Right $ Just fullPath
                else return $ Right Nothing
        `catchAll` \e -> return $ Left $ show e

unwrapOrPrintError :: Lock -> Either [Char] (Maybe a) -> IO (Maybe a)
unwrapOrPrintError lock (Left e) = do
    Lock.with lock $ putStrLn ("*** " ++ e)
    return Nothing
unwrapOrPrintError _ (Right x) = return x

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
