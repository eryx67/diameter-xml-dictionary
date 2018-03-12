-- | 

module Network.Diameter.XmlDictionary.CsvParser (decodeAVP
                                                ) where


import qualified Data.Csv as Csv
import Data.Vector (Vector)
import Data.ByteString.Lazy (ByteString)
import Data.Bifunctor (second)

import Network.Diameter.XmlDictionary.Types (AVP)

decodeAVP :: ByteString -> Either String (Vector AVP)
decodeAVP =
  second snd . Csv.decodeByName

