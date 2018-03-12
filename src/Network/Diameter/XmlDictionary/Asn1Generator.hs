{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
-- |

module Network.Diameter.XmlDictionary.Asn1Generator (generateASN1
                                                    ) where

import Data.Maybe (isJust, fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Map.Strict as Map
import Text.Heterocephalus
import Text.Blaze (Markup,)
import Text.Blaze.Renderer.Text (renderMarkup)

import Network.Diameter.XmlDictionary.Types

class ToAsn1Markup a where
  toAsn1Markup :: a -> Markup

generateASN1 :: ToAsn1Markup a => a -> TL.Text
generateASN1 =
  renderMarkup . toAsn1Markup

instance ToAsn1Markup Application where
  toAsn1Markup a@Application{..} =
    let an = fromMaybe "Application" applicationName
        nes = zip [(0::Int)..] $ applicationExports a <> map commandName applicationCommands
    in
      [compileText|#{an} DEFINITIONS IMPLICIT TAGS ::= BEGIN
 EXPORTS
     %{forall (n, e) <- nes}
     %{if n /= 0},%{endif}#{e}
     %{endforall};

 IMPORTS
 %{forall ai <- applicationAvpImports}
 #{toAsn1Markup ai}

 %{endforall};

 %{forall avp <- applicationAvps}
 #{toAsn1Markup avp}

%{endforall}

 %{forall c <- applicationCommands}
 #{toAsn1Markup c}

%{endforall}
 END
 |]

instance ToAsn1Markup AvpImport where
  toAsn1Markup AvpImport{..} =
    let tns = zip [(0::Int)..] avpImportTypeNames
    in
      [compileText|
     %{forall (n, e) <- tns}
     %{if n /= 0},%{endif}#{e}
     %{endforall}
 FROM #{avpImportApp}
 |]

instance ToAsn1Markup AVP where
  toAsn1Markup AVP{..} =
    let mbit :: Text = if avpMandatory == Just Must then "TRUE" else "FALSE"
        vbit :: Text = if isJust avpVendorId then "TRUE" else "FALSE"
        pbit :: Text = if avpProtected == Just Must then "TRUE" else "FALSE"
    in
    [compileText|#{avpName} ::= [UNIVERSAL #{avpCode}] SEQUENCE {
 avpFlags AVP-Flags DEFAULT {v_bit #{vbit}, m_bit #{mbit}, p_bit #{pbit}},
 %{ case avpVendorId }
 %{ of Just vid }
 vendorId Vendor-ID DEFAULT #{vid},
 %{ of Nothing }
 %{ endcase }
 data     #{ toAsn1Markup avpType }
 }|]

instance ToAsn1Markup Command where
  toAsn1Markup c@Command{..} =
    let ars = toAsn1Markup $ case c of
          Command{commandAnswerRules = AnswerRules (AvpRules [] [] [])} ->
            requestRules commandRequestRules
          _ ->
            answerRules commandAnswerRules
    in
      [compileText|#{commandName} ::= [UNIVERSAL #{commandCode}] SEQUENCE {
 #{ars}
 }|]

instance ToAsn1Markup AvpType where
  toAsn1Markup (AvpType n) =
    [compileText|#{n}|]
  toAsn1Markup (AvpEnum kvs) =
    let kvs' = zip [(0::Int)..] $ Map.toList kvs
    in
      [compileText|ENUMERATED {
    %{ forall (n, (k,v)) <- kvs' }
    %{ if n /= 0 },%{ endif } #{enumMemberName k} (#{v})
    %{ endforall }
    }|]
  toAsn1Markup (AvpGrouped ars) =
    [compileText|SEQUENCE {
    #{toAsn1Markup ars}
    }
    |]

instance ToAsn1Markup AvpRules where
  toAsn1Markup AvpRules{..} =
    let [fixed, required, optional] =
          map (zip [(0::Int)..]) [avpRulesFixed, avpRulesRequired, avpRulesOptional]
    in
      [compileText|
 %{ if fixed /= [] }
 fixed [0] SEQUENCE {
 %{ forall (n, r) <- fixed }
 %{ if n /= 0 },%{ endif } #{toAsn1Markup r}
 %{ endforall }
 }
 %{ endif }
%{ if required /= [] } %{ if fixed /= [] },%{endif}
 required [1] SET {
 %{ forall (n, r) <- required }
 %{ if n /= 0 },%{ endif }#{toAsn1Markup r}
 %{ endforall }
 }
 %{ endif }
%{ if optional /= [] }%{ if required /= [] },%{endif}
 optional [2] SET {
 %{ forall (n, r) <- optional }
 %{ if n /= 0 },%{ endif }#{toAsn1Markup r} OPTIONAL
 %{ endforall }
 }
 %{ endif }
|]

instance ToAsn1Markup AvpRule where
  toAsn1Markup AvpRule{..} =
    case (avpRuleMinimum, avpRuleMaximum) of
      (Just 1, Just 1) ->
        [compileText|#{attrName avpRuleName} #{avpRuleName}|]
      (Nothing, Nothing) ->
        [compileText|#{attrName avpRuleName} SET OF #{avpRuleName} |]
      (Nothing, Just mx) ->
        [compileText|#{attrName avpRuleName} SET (SIZE ((0::Int)..#{mx})) OF #{avpRuleName} |]
      (Just mn, Nothing) ->
        [compileText|#{attrName avpRuleName} SET (SIZE (#{mn}..MAX)) OF #{avpRuleName} |]
      (Just mn, Just mx) ->
        [compileText|#{attrName avpRuleName} SET (SIZE (#{mn}..#{mx})) OF #{avpRuleName} |]

attrName :: Text -> Text
attrName v =
  let (f, r) = T.splitAt 1 v
  in
    T.toLower f <> T.replace "-" "_" r

enumMemberName :: Text -> Text
enumMemberName =
    T.toLower . T.replace "-" "_"
