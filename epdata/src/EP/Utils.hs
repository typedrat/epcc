module EP.Utils ( embedYAML, makeRelativeToProject ) where

import Data.Aeson.Types
import Data.FileEmbed
import qualified Data.Yaml as YAML
import Language.Haskell.TH.Syntax

embedYAML :: forall a. (Lift a, FromJSON a) => FilePath -> Q Exp
embedYAML path = lift =<< YAML.decodeFileThrow @_ @a path
