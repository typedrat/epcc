{-# LANGUAGE TemplateHaskell #-}
module EP.Schema.Source ( Book(..), Source(..), HasSource(..) ) where

import Control.Lens.TH
import Data.Aeson.Encoding
import Data.Aeson.Types
import Data.Char           ( toLower )
import qualified Data.Text as T
import GHC.Generics        ( Generic )

data Book = EclipsePhase
          | Transhuman
          | Sunward
          | Rimward
          | Gatecrashing
          | Panopticon
          | Firewall
          deriving (Show, Eq, Ord, Generic)

instance FromJSON Book where
    parseJSON = withText "Book" $ \t ->
        case T.unpack t of
            "Eclipse Phase" -> pure EclipsePhase
            "Transhuman"    -> pure Transhuman
            "Sunward"       -> pure Sunward
            "Rimward"       -> pure Rimward
            "Gatecrashing"  -> pure Gatecrashing
            "Panopticon"    -> pure Panopticon
            "Firewall"      -> pure Firewall

instance ToJSON Book where
    toJSON EclipsePhase = String "Eclipse Phase"
    toJSON Transhuman   = String "Transhuman"
    toJSON Sunward      = String "Sunward"
    toJSON Rimward      = String "Rimward"
    toJSON Gatecrashing = String "Gatecrashing"
    toJSON Panopticon   = String "Panopticon"
    toJSON Firewall     = String "Firewall"

    toEncoding EclipsePhase = text "Eclipse Phase"
    toEncoding Transhuman   = text "Transhuman"
    toEncoding Sunward      = text "Sunward"
    toEncoding Rimward      = text "Rimward"
    toEncoding Gatecrashing = text "Gatecrashing"
    toEncoding Panopticon   = text "Panopticon"
    toEncoding Firewall     = text "Firewall"

data Source = Source { _srcBook :: Book, _srcPage :: Word }
            deriving (Show, Eq, Ord, Generic)

sourceOptions :: Options
sourceOptions = defaultOptions { fieldLabelModifier = fmap toLower . drop 4 }

instance FromJSON Source where
    parseJSON = genericParseJSON sourceOptions

instance ToJSON Source where
    toJSON = genericToJSON sourceOptions
    toEncoding = genericToEncoding sourceOptions

makeClassy ''Source
