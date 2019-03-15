{-# LANGUAGE OverloadedStrings #-}

import           Test.Hspec

import           Lib

import           Data.Either

main :: IO ()
main = hspec $ do
  describe "fileNameFromUrl" $ do
    it "parses filename from valid url"
      $          fileNameFromUrl
                   (Url
                     "https://tenhou.net/0/log/find.cgi?log=2019031108gm-00d1-0000-4e6038fa&tw=0"
                   )
      `shouldBe` Right "2019031108gm-00d1-0000-4e6038fa&tw=0.mjlog"

    it "fails gracefully on invalid url" $ do
      fileNameFromUrl (Url "https://tenhou.net/0/log/find.cgi")
        `shouldSatisfy` isLeft

      fileNameFromUrl
          (Url
            "https://tenhou.net/0/log/find.cgi?l=2019031108gm-00d1-0000-4e6038fa&tw=0"
          )
        `shouldSatisfy` isLeft
