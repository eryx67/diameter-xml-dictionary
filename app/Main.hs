{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import qualified Data.Text as T
import Options.Applicative.Simple
import qualified Data.Text.Lazy.IO as TL

import Network.Diameter.XmlDictionary
import Network.Diameter.XmlDictionary.Asn1Generator

data CommandType = Xml
  | Asn1

opts :: IO ((ParseConfig, String, String), CommandType)
opts = simpleOptions "1.0.0"
  "Generate Diameter XML dictionary"
  "Program takes csv file for AVPs and BNF file for commands, grouped and enumerated AVPs \n\
  \and write diameter xml dictionary to output"
  (gatherOpts <$> option auto (long "application-id" <> metavar "AID" <> help "application id")
    <*> strOption (long "application-name" <> metavar "ANAME" <> help "application name")
    <*> option auto (long "vendor-id" <> metavar "VID" <> help "vendor id")
    <*> strOption (long "vendor-name" <> metavar "VNAME" <> help "vendor name")
    <*> strOption (long "csv" <> metavar "CSV" <> help "CSV file with AVPs descriptions")
    <*> strOption (long "bnf" <> metavar "BNF" <> help "BNF file with commands, enumerated and grouped AVPs descriptions")) $ do
  addCommand "xml"
    "Generate xml dictionary"
    (const Xml)
    (pure ())
  addCommand "asn1"
    "Generate xml dictionary"
    (const Asn1)
    (pure ())
  where
    gatherOpts aid an vid vn csv bnf =
      (ParseConfig aid an vid vn bnf, T.unpack csv, T.unpack bnf)

main :: IO ()
main = do
  ((pcfg, csvFName, bnfFName), cmdName) <- opts
  csv <- readFile csvFName
  bnf <- readFile bnfFName
  let app = parseApplication pcfg csv bnf
      cmd = case cmdName of
        Xml -> generateXML pcfg
        Asn1 -> generateASN1

  either error (TL.putStr . cmd) app
