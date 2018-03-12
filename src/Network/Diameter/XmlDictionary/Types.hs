{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Network.Diameter.XmlDictionary.Types where

import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Default.Class (Default(..))
import Data.Monoid ((<>), mconcat)
import Data.Foldable (msum)
import Data.List ((\\))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Data.Map.Strict (empty)
import qualified Data.Csv as Csv
import Data.Csv ((.:))
import Text.Blaze (ToMarkup(..))
import Text.XML (Document(..)
                , Doctype(..)
                , Prologue(..)
                , Node(..)
                , Element(..)
                , ExternalID(..)
                , Name(..)
                )

type VendorId = Integer
type ApplicationId = Integer

class ToXmlNodes a where
  toXmlNodes :: a -> [Node]

data Dictionary = Dictionary {
  dictVendors :: ![Vendor]
  , dictBases :: ![Base]
  , dictApplications :: ![Application]
  }
  deriving Show

instance ToMarkup Dictionary where
  toMarkup Dictionary{..} =
    toMarkup $ document nodes
    where
      nodes =
        concat $
        map toXmlNodes dictVendors <>
        map toXmlNodes dictBases <>
        map toXmlNodes dictApplications
  preEscapedToMarkup Dictionary{..} =
    preEscapedToMarkup $ document nodes
    where
      nodes =
        concat $
        map toXmlNodes dictVendors <>
        map toXmlNodes dictBases <>
        map toXmlNodes dictApplications

data Vendor = Vendor {
  vendorId :: !Integer
  , vendorName :: !Text
  }
  deriving Show

instance ToXmlNodes Vendor where
  toXmlNodes Vendor{..} =
    pure $ elNode "vendor" ("id" @= tshow vendorId <> "name" @= vendorName) []

data Base = Base {
  baseUri :: !(Maybe Text)
  , baseCommands :: ![Command]
  , baseTypedefns :: ![Typedefn]
  , baseAvps :: ![AVP]
  }
  deriving Show

instance ToXmlNodes Base where
  toXmlNodes Base{..} =
    pure $ elNode "base" attrs nodes
    where
      attrs =
        "uri" @=? baseUri
      nodes =
         concat $
         map toXmlNodes baseCommands <>
         map toXmlNodes baseTypedefns <>
         map toXmlNodes baseAvps

data Application = Application {
  applicationId :: !Integer
  , applicationName :: !(Maybe Text)
  , applicationUri :: !(Maybe Text)
  , applicationCommands :: ![Command]
  , applicationTypedefns :: ![Typedefn]
  , applicationAvps :: ![AVP]
  , applicationAvpImports :: ![AvpImport]
  }
  deriving Show

-- | AVP names application exports
applicationExports :: Application -> [Text]
applicationExports Application{..} =
  [avpName | AVP{..} <- applicationAvps]

applicationImports :: Application -> [Text]
applicationImports a@Application{..} =
  allTypes \\ applicationExports a
  where
    allTypes = Set.toList . Set.fromList . mconcat $
      map commandAvpTypes applicationCommands <> map avpTypes applicationAvps
    commandAvpTypes Command{commandRequestRules = (RequestRules rs),
                            commandAnswerRules = AnswerRules as
                           } =
      avpRulesTypes rs <> avpRulesTypes as
    avpRulesTypes AvpRules{..} =
      map avpRuleType $ avpRulesFixed <> avpRulesRequired <> avpRulesOptional
    avpRuleType AvpRule{..} =
      avpRuleName
    avpTypes AVP{..} = avpName:avpTypeTypes avpType
    avpTypeTypes (AvpType n) = [n]
    avpTypeTypes AvpEnum{} = []
    avpTypeTypes (AvpGrouped rs) =
      avpRulesTypes rs
      
instance ToXmlNodes Application where
  toXmlNodes Application{..} =
    pure $ elNode "application" attrs nodes
    where
      attrs =
        "id" @= tshow applicationId <>
        "uri" @=? applicationUri <>
        "name" @=? applicationName
      nodes =
        concat $
        map toXmlNodes applicationCommands <>
        map toXmlNodes applicationTypedefns <>
        map toXmlNodes applicationAvps

data Command = Command {
  commandName :: !Text
  , commandCode :: !Integer
  , commandVendorId :: !(Maybe Integer)
  , commandPbit :: !(Maybe CommandPbit)
  , commandRequestRules :: !RequestRules
  , commandAnswerRules :: !AnswerRules
  }
  deriving Show

data CommandPbit =
  PbitZero
  | PbitOne
  deriving (Show, Eq)

instance Default CommandPbit where
  def = PbitOne

instance ToXmlNodes Command where
  toXmlNodes Command{..} =
    pure $ elNode "command" attrs nodes
    where
      nodes =
        toXmlNodes commandRequestRules <>
        toXmlNodes commandAnswerRules
      attrs =
        "name" @= commandName <>
        "code" @= tshow commandCode <>
        "vendor-id" @=? fmap tshow commandVendorId <>
        "pbit" @= pbit
      pbit = case fromMaybe def commandPbit of
        PbitZero -> "0"
        PbitOne -> "1"

data Typedefn = Typedefn {
  typedefnTypeName :: !Text
  , typedefnTypeParent :: !(Maybe Text)
  , typedefnDescription :: !(Maybe Text)
  }
  deriving Show


instance ToXmlNodes Typedefn where
  toXmlNodes Typedefn{..} =
    pure $ elNode "typedfn" attrs []
    where
      attrs =
        "type-name" @= typedefnTypeName <>
        "type-parent" @=? typedefnTypeParent <>
        "description" @=? typedefnDescription

data AVP = AVP {
  avpName :: !Text
  , avpDescription :: !(Maybe Text)
  , avpCode :: !Integer
  , avpMandatory :: !(Maybe Mandatory)
  , avpProtected :: !(Maybe Mandatory)
  , avpMayEncrypt :: !(Maybe Bool)
  , avpVendorId :: !(Maybe Integer)
  , avpType :: !AvpType
  }
  deriving Show

instance ToXmlNodes AVP where
  toXmlNodes AVP{..} =
    pure . elNode "avp" attrs $ toXmlNodes avpType
    where
      attrs =
        "name" @= avpName <>
        "description" @=? avpDescription <>
        "code" @= tshow avpCode <>
        "mandatory" @= (tshow $ fromMaybe def avpMandatory) <>
        "protected" @= (tshow $ fromMaybe def avpProtected) <>
        "vendor-id" @=? fmap tshow avpVendorId <>
        "may-encrypt" @= (encryptVal $ fromMaybe False avpMayEncrypt)
      encryptVal True = "yes"
      encryptVal False = "no"

data Mandatory =
  Must
  | May
  | MustNot
  | ShouldNot
  deriving Eq

instance Csv.FromNamedRecord AVP where
    parseNamedRecord m = AVP <$> m .: "Attribute Name"
      <*> m .: "Section"
      <*> m .: "Code"
      <*> mandatory 'M'
      <*> mandatory 'P'
      <*> (protected <$> m .: "Encr")
      <*> vendor
      <*> avptype
      where
        avptype = m .: "Data" >>= \case
          "Enumerated" -> return $ AvpEnum empty
          "Grouped" -> return $ AvpGrouped def
          tp -> return $ AvpType tp
        vendor = do
          v <- mandatory 'V'
          return $ case v of
                     Just Must -> Just 0
                     Just May -> Just 0
                     _ -> Nothing
        protected s =
          if 'Y' `elem` (s::String) then Just True else Nothing
        mandatory c =
          fmap msum . sequence $ uncurry (checkMandatory c) <$>
          [("MUST", Must),("MAY", May),("SHLD NOT", ShouldNot),("MUST NOT", MustNot)]
        checkMandatory c n v =
          m .: n >>= \f ->
          return $
          if c `elem` (f :: String)
          then Just v
          else Nothing

instance Csv.DefaultOrdered AVP where
    headerOrder _ = Csv.header ["Attribute Name","Code","Section","Data","MUST","MAY","SHLD NOT","MUST NOT","Encr","Applicability"]

instance Show Mandatory where
  show Must = "must"
  show May = "may"
  show MustNot = "mustnot"
  show ShouldNot = "shouldnot"

instance Default Mandatory where
  def = May

data AvpType =
  AvpType !Text
  | AvpGrouped !AvpRules
  | AvpEnum !(Map.Map Text Integer)
  deriving Show

instance ToXmlNodes AvpType where
  toXmlNodes (AvpType name) =
    pure $ elNode "type" ("type-name" @= name) []
  toXmlNodes (AvpGrouped rules) =
    pure . elNode "grouped" empty $ toXmlNodes rules
  toXmlNodes (AvpEnum enum) =
    pure . elNode "type" ("type-name" @= "Enumerated") $
    uncurry enode <$> Map.toList enum
    where
      enode k v =
        elNode "enum" ("name" @= k <> "code" @= tshow v) []

data AvpImport = AvpImport {
  avpImportApp :: !Text
  , avpImportTypeNames :: ![Text]
  }
  deriving (Show, Eq)

data RequestRules = RequestRules {requestRules :: !AvpRules}
  deriving Show

instance ToXmlNodes RequestRules where
  toXmlNodes (RequestRules rules) =
    case toXmlNodes rules of
      [] ->
        []
      nodes ->
        pure $ elNode "requestrules" empty nodes

data AnswerRules = AnswerRules {answerRules :: !AvpRules}
  deriving Show

instance ToXmlNodes AnswerRules where
  toXmlNodes (AnswerRules rules) =
    case toXmlNodes rules of
      [] ->
        []
      nodes ->
        pure $ elNode "answerrules" empty nodes

data AvpRules = AvpRules{
  avpRulesFixed :: ![AvpRule]
  , avpRulesRequired :: ![AvpRule]
  , avpRulesOptional :: ![AvpRule]
  }
  deriving (Show, Eq)

instance Default AvpRules where
  def = AvpRules [] [] []

instance ToXmlNodes AvpRules where
  toXmlNodes AvpRules{..} =
    avprulesToXmlNodes "fixed" avpRulesFixed <>
    avprulesToXmlNodes "required" avpRulesRequired <>
    avprulesToXmlNodes "optional" avpRulesOptional

avprulesToXmlNodes :: ToXmlNodes a => Text -> [a] -> [Node]
avprulesToXmlNodes _ [] = []
avprulesToXmlNodes name rules =
  pure . elNode name empty . concat $ toXmlNodes <$> rules

data AvpRule = AvpRule {
  avpRuleName :: !Text
  , avpRulePosition :: !(Maybe RulePosition)
  , avpRuleMinimum :: !(Maybe Integer)
  , avpRuleMaximum :: !(Maybe Integer)
  }
  deriving (Show, Eq)

instance ToXmlNodes AvpRule where
  toXmlNodes AvpRule{..} =
    pure $ elNode "avprule" ("name" @= avpRuleName <>
                             "position" @= tshow (fromMaybe def avpRulePosition) <>
                             "maximum" @=? fmap tshow avpRuleMaximum <>
                             "minimum" @=? fmap tshow avpRuleMinimum
                            ) []

data RulePosition =
  First
  | Last
  | Unspecified
  deriving Eq

instance Show RulePosition where
  show First = "first"
  show Last = "last"
  show Unspecified = "unspecified"

instance Default RulePosition where
  def = Unspecified

document :: [Node] -> Document
document nodes = Document {documentPrologue = docPrologue
                          , documentEpilogue = []
                          , documentRoot = dictionary nodes
                          }

docPrologue :: Prologue
docPrologue = Prologue { prologueBefore = []
                       , prologueDoctype = Just . Doctype "dictionary" . Just $ SystemID "dictionary.dtd"
                       , prologueAfter = []
                       }

dictionary :: [Node] -> Element
dictionary =
  el "dictionary" empty

elNode :: Text -> Map.Map Name Text -> [Node] -> Node
elNode name attrs =
  NodeElement . el name attrs

el :: Text -> Map.Map Name Text -> [Node] -> Element
el name =
  Element (elName name)

elName :: Text -> Name
elName txt =
  Name txt Nothing Nothing

infixl 9 @=
(@=) :: Name -> Text -> Map.Map Name Text
(@=) = Map.singleton

infixl 9 @=?
(@=?) :: Name -> Maybe Text -> Map.Map Name Text
(@=?) k = maybe empty (Map.singleton k)

tshow :: Show a => a -> Text
tshow = T.pack . show
