{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Either (isRight)
import qualified Data.Text as T
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy as TL

import Network.Diameter.XmlDictionary
import Network.Diameter.XmlDictionary.Asn1Generator
import Network.Diameter.XmlDictionary.Types (VendorId, ApplicationId)


dataDir :: FilePath
dataDir = "test-data"

specName :: String
specName = "3GPP_TS_29_214"

vendorId :: VendorId
vendorId = 10415

vendorName :: Text
vendorName = "3GPP"

applicationId :: ApplicationId
applicationId = 16777236

applicationName :: Text
applicationName = "3GPP-29-214"

parseConfig :: ParseConfig
parseConfig = ParseConfig applicationId applicationName vendorId vendorName (T.pack $ testFName "bnf")

main :: IO ()
main = hspec $ before readSrc $
  describe "generate Diameter dictionaries" $ do
    it "parse bnf and csv files to Application" $ \(csv, bnf) ->
      parseApplication parseConfig csv bnf `shouldSatisfy` isRight
    it "generate XML dictionary" $ \srcs -> do
      testData <- removeSpaces <$> TL.readFile (testFName "xml")
      either error (pure . removeSpaces . generateXML parseConfig) (parseApp srcs) `shouldReturn` testData
    it "generate ASN1 dictionary" $ \srcs -> do
      testData <- removeSpaces <$> TL.readFile (testFName "asn1")
      either error (pure . removeSpaces . generateASN1) (parseApp srcs) `shouldReturn` testData
  where
    parseApp =
      uncurry $ parseApplication parseConfig
    readSrc =
      (,) <$> readFile (testFName "csv") <*> readFile (testFName "bnf")

testFName :: String -> FilePath
testFName sf =
  dataDir </> specName <.> sf

removeSpaces :: TL.Text -> TL.Text
removeSpaces =
  TL.filter (`notElem` (" \t\r\n"::String))

(</>) :: FilePath -> FilePath -> FilePath
(</>) fp1 fp2 =
  fp1 <> "/" <> fp2

infixr 5 </>

(<.>) :: FilePath -> String -> FilePath
(<.>) fp1 sf =
  fp1 <> "." <> sf

infixr 5 <.>
