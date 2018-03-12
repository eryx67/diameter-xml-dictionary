{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module Network.Diameter.XmlDictionary (generateXML
                                      , parseApplication
                                      , ParseConfig(..)
                                      ) where

import Data.Monoid ((<>))
import Data.Default.Class
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Map.Strict ((!?))
import qualified Text.XML as XML
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Vector as V

import Network.Diameter.XmlDictionary.Types
import Network.Diameter.XmlDictionary.BnfParser
import Network.Diameter.XmlDictionary.CsvParser (decodeAVP)

data ParseConfig = ParseConfig {
  pcApplicationId :: !ApplicationId
  , pcApplicationName :: !Text
  , pcVendorId :: !VendorId
  , pcVendorName :: !Text
  , pcSourceName :: !Text
  }
  deriving Show

instance Default ParseConfig where
  def = ParseConfig 123 "test-app" 456 "test-vendor" "dictionary"

generateXML :: ParseConfig -> Application -> TL.Text
generateXML ParseConfig{..} app =
  let vendor = Vendor pcVendorId pcVendorName
      nodes = toXmlNodes vendor <> toXmlNodes app
      doc = document nodes
      rs = def{XML.rsPretty = True, XML.rsXMLDeclaration = True}
  in
    XML.renderText rs doc

parseApplication :: ParseConfig -> String -> String -> Either String Application
parseApplication ParseConfig{..} csv bnf = do
  avps <- decodeAVP $ BL.pack csv
  (cmds, bnfAvps, is) <- parseBnf (Just pcVendorId) (T.unpack pcSourceName) bnf

  let
      bnfAvpDict = foldr (\av acc -> Map.insert (avpCode av) av acc) Map.empty bnfAvps
      avps' = map (delAvpAuxAvpRule . fillAvpFromVendor . fillAvpFromBnf bnfAvpDict) $
              V.toList avps
      cmds' = map delCmdAuxAvpRule cmds
  return $ Application pcApplicationId (Just pcApplicationName) Nothing cmds' [] avps' is
  where
    delAvpAuxAvpRule avp@AVP{avpType = AvpGrouped ars} =
      avp{avpType = AvpGrouped $ delAuxAvpRule ars}
    delAvpAuxAvpRule avp =
      avp
    delCmdAuxAvpRule c@Command{..} =
      let RequestRules rs = commandRequestRules
          AnswerRules as = commandAnswerRules
      in
      c{commandRequestRules = RequestRules $ delAuxAvpRule rs
       , commandAnswerRules = AnswerRules $ delAuxAvpRule as
       }
    delAuxAvpRule ars@AvpRules{..} =
      ars{avpRulesOptional = filter (\ar -> avpRuleName ar /= "AVP") avpRulesOptional}
    fillAvpFromBnf bnfAvps avp  =
      case bnfAvps !? avpCode avp of
        Just AVP{avpType = avt} ->
          avp{avpType = avt}
        Nothing ->
          avp
    fillAvpFromVendor avp@AVP{..} =
      avp{avpVendorId = pcVendorId <$ avpVendorId}
