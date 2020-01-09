module EP.Schema.Predicate ( Predicate(..), _HasTraitNamed, _Not ) where

import Control.Lens.TH
import Data.Aeson.Types
import qualified Data.Text        as T
import GHC.Generics               ( Generic )
import Language.Haskell.TH.Syntax ( Lift )

data Predicate = HasTraitNamed T.Text
               | Not [Predicate]
               deriving (Show, Eq, Ord, Generic, Lift)

makePrisms ''Predicate

predicateOptions :: Options
predicateOptions = defaultOptions { sumEncoding = ObjectWithSingleField
                                  , constructorTagModifier = camelTo2 '_'
                                  }

instance FromJSON Predicate where
    parseJSON = genericParseJSON predicateOptions

instance ToJSON Predicate where
    toJSON = genericToJSON predicateOptions
    toEncoding = genericToEncoding predicateOptions
