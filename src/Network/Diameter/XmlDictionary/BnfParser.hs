{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
-- |

module Network.Diameter.XmlDictionary.BnfParser (parseBnf
                                                , bnfParser
                                                , commandDef
                                                , groupedAvpDef
                                                , enumeratedAvpDef
                                                ) where

import qualified Data.Map.Strict as Map
import Data.Functor (void)
import Data.Bifunctor (first)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.Default.Class (def)
import qualified Text.Parsec as Parsec
import qualified Data.Text as T
import Data.Text (Text)
import Text.Parsec (SourceName
                   , (<?>), (<|>)
                   , oneOf, many, alphaNum, string, char, between
                   , optionMaybe, option, label, eof)
import Text.Parsec.String (GenParser)
import Text.Parsec.Extra (natural, eol)

import Network.Diameter.XmlDictionary.Types (Command(Command)
                                            , CommandPbit(..)
                                            , AvpRules(..)
                                            , AvpRule(..)
                                            , RequestRules(..)
                                            , AnswerRules(..)
                                            , AVP(AVP)
                                            , AvpType(..)
                                            , VendorId
                                            , AvpImport(..))

data CommandBit = REQ | PXY | ERR
  deriving (Show, Eq)

data CommandHeader = CommandHeader {
  chCommandId :: !Integer
  , chApplicationId :: !(Maybe Integer)
  , chCommandBits :: ![CommandBit]
  }
  deriving (Show, Eq)

data AvpHeader = AvpHeader {
  ahAvpId :: !Integer
  }
  deriving (Show, Eq)

data Qual =
  Qual !(Maybe Integer) !(Maybe Integer)
  deriving (Eq, Show)

data RuleType =
  Fixed AvpRule
  | Required AvpRule
  | Optional AvpRule
  deriving Show

data ParserState = ParserState {
  psCommands :: ![Command]
  , psAvps :: ![AVP]
  , psAvpImports :: ![AvpImport]
  }
  deriving Show

parseBnf :: Maybe VendorId -> SourceName -> String -> Either String ([Command], [AVP], [AvpImport])
parseBnf vid sn s =
  first show $ Parsec.runParser (bnfParser vid) (ParserState [] [] []) sn s

bnfParser :: Maybe VendorId -> GenParser Char ParserState ([Command], [AVP], [AvpImport])
bnfParser vid = do
  let cmd = Parsec.try (commandDef vid) >>= \c ->
        Parsec.modifyState (\ps -> ps{psCommands = c:psCommands ps})
      gavp = Parsec.try (groupedAvpDef vid) >>= \a ->
        Parsec.modifyState (\ps -> ps{psAvps = a:psAvps ps})
      eavp = Parsec.try (enumeratedAvpDef vid) >>= \a ->
        Parsec.modifyState (\ps -> ps{psAvps = a:psAvps ps})
      avpi = avpImportDef >>= \i ->
        Parsec.modifyState (\ps -> ps{psAvpImports = i:psAvpImports ps})

  do
    void . many $ do
      Parsec.spaces
      cmd <|> gavp <|> eavp <|> avpi
    eof
    Parsec.getState >>= \(ParserState cs avps is) ->
      return (cs, avps, is)

commandDef :: Maybe VendorId -> GenParser Char st Command
commandDef vid = do
  cn <- parens "<" ">" commandName <?> "command name"
  void $ string "::="
  (CommandHeader{..}, rs) <- diameterMessage commandHeader
  let pb = if PXY `elem` chCommandBits
           then Just PbitOne
           else Just PbitZero
      ars = AvpRules [rl | Fixed rl <- rs] [rl | Required rl <- rs] [rl | Optional rl <- rs]
      (reqRs, ansRs) = if REQ `elem` chCommandBits
                       then (RequestRules ars, AnswerRules def)
                       else (RequestRules def, AnswerRules ars)
  return $ Command (T.pack cn) chCommandId vid pb reqRs ansRs

groupedAvpDef :: Maybe Integer -> GenParser Char st AVP
groupedAvpDef vid = do
  an <- token avpDefName <?> "avp name"
  void $ string "::="
  (AvpHeader{..}, rs) <- diameterMessage groupedAvpHeader
  let at = AvpGrouped $
        AvpRules [rl | Fixed rl <- rs] [rl | Required rl <- rs] [rl | Optional rl <- rs]
  return $ AVP (T.pack an) Nothing ahAvpId Nothing Nothing Nothing vid at

enumeratedAvpDef :: Maybe Integer -> GenParser Char st AVP
enumeratedAvpDef vid = do
  an <- token avpDefName <?> "avp name"
  void $ string "::="
  AvpHeader{..} <- enumeratedAvpHeader
  eol
  ems <- many $ Parsec.try enumMember
  let at = AvpEnum . Map.fromList $ [(n, fromMaybe i v) | ((n, v), i) <- zip ems [0..]]
  return $ AVP (T.pack an) Nothing ahAvpId Nothing Nothing Nothing vid at

enumMember :: GenParser Char st (Text, Maybe Integer)
enumMember = flip label "enum member" $ do
  spaces
  res <- parens "<" ">" ((,) <$> (T.pack <$> diameterName)
                         <*> optionMaybe (Parsec.try memberVal))
  eol
  return res
  where
    memberVal =
       spaces *> string ":" *> spaces *> natural

avpImportDef :: GenParser Char st AvpImport
avpImportDef = do
  an <- T.pack <$> token avpDefName <?> "import application name"
  void $ string "::="
  void avpImportHeader
  eol
  ims <- many $ Parsec.try importMember
  return $ AvpImport an ims

importMember :: GenParser Char st Text
importMember = flip label "import avp name" $ do
  spaces
  res <- parens "<" ">" $ T.pack <$> diameterName
  eol
  return res

commandName :: GenParser Char st String
commandName =
  diameterName <?> "command name"

avpDefName :: GenParser Char st String
avpDefName =
  diameterName <?> "avp name"

diameterName :: GenParser Char st String
diameterName = (:) <$> alphaNum
  <*> many (alphaNum <|> char '-')

diameterMessage :: GenParser Char st h -> GenParser Char st (h, [RuleType])
diameterMessage header = do
  h <- header
  eol
  frs <- many $ Parsec.try fixed
  rs <- many (Parsec.try required <|> optional)
  return (h, frs <> rs)

groupedAvpHeader :: GenParser Char st AvpHeader
groupedAvpHeader =
  parens "<AVP-Header:" ">" $ do
  ai <- avpId
  return $ AvpHeader ai

enumeratedAvpHeader :: GenParser Char st AvpHeader
enumeratedAvpHeader =
  parens "<AVP-Enumerated:" ">" $ do
  ai <- avpId
  return $ AvpHeader ai

avpImportHeader :: GenParser Char st ()
avpImportHeader =
  parens "<AVP-Import" ">" spaces

--   header           = "<Diameter-Header:" command-id
--                          [r-bit] [p-bit] [e-bit] [application-id]">"
commandHeader :: GenParser Char st CommandHeader
commandHeader =
  parens "<Diameter-Header:" ">" $ do
  ci <- commandId
  rb <- option [] $ pure <$> Parsec.try rbit
  pb <- option [] $ pure <$> Parsec.try pbit
  eb <- option [] $ pure <$> ebit
  ai <- optionMaybe applicationId
  let bits = rb <> pb <> eb
  return $ CommandHeader ci ai bits

--    fixed            = [qual] "<" avp-spec ">"
fixed :: GenParser Char st RuleType
fixed =
  Fixed <$> avpRule "<" ">"

--    required         = [qual] "{" avp-spec "}"
required :: GenParser Char st RuleType
required =
  Required <$> avpRule "{" "}"

--    optional         = [qual] "[" avp-name "]"
optional :: GenParser Char st RuleType
optional =
  Optional <$> avpRule "[" "]"

avpRule :: String -> String -> GenParser Char st AvpRule
avpRule ob cb = flip label "avp rule" $ do
  (Qual min' max') <- option (Qual (Just 1) (Just 1)) qual
  spaces
  avps <- parens ob cb avpSpec <?> "avp name"
  eol
  return $ AvpRule (T.pack avps) Nothing min' max'

--    avp-spec         = diameter-name
avpSpec :: GenParser Char st String
avpSpec =
  diameterName

--    qual             = [min] "*" [max]
qual :: GenParser Char st Qual
qual = flip label "qual" $ do
  min' <- optionMaybe natural
  void $ char '*'
  max' <- optionMaybe natural
  return $ Qual min' max'

avpId :: GenParser Char st Integer
avpId =
  commandId

--    command-id       = 1*DIGIT
commandId :: GenParser Char st Integer
commandId =
  token natural

--    application-id   = 1*DIGIT
applicationId :: GenParser Char st Integer
applicationId = commandId <?> "application id"

--    r-bit            = ", REQ"
--                       ; If present, the 'R' bit in the Command
--                       ; Flags is set, indicating that the message
--                       ; is a request as opposed to an answer.
rbit :: GenParser Char st CommandBit
rbit =
  token (string ", REQ") *> pure REQ

--    p-bit            = ", PXY"
--                       ; If present, the 'P' bit in the Command
--                       ; Flags is set, indicating that the message
--                       ; is proxiable.
pbit :: GenParser Char st CommandBit
pbit =
  token (string ", PXY") *> pure PXY

--    e-bit            = ", ERR"
--                       ; If present, the 'E' bit in the Command
--                       ; Flags is set, indicating that the answer
--                       ; message contains a Result-Code AVP in
--                       ; the "protocol error" class.
ebit :: GenParser Char st CommandBit
ebit =
  token (string ", ERR") *> pure ERR

parens :: String -> String -> GenParser Char st a -> GenParser Char st a
parens ob cb =
  between (symbol ob) (symbol cb)

symbol :: String -> GenParser Char st String
symbol s =
  spaces *> string s <* spaces

token :: GenParser Char st a -> GenParser Char st a
token p =
  p <* spaces

spaces :: GenParser Char st ()
spaces =
  Parsec.skipMany $ oneOf " \t"
