module EP.Utils ( embedYAML, embedYAMLNameMap ) where

import Control.Arrow             ( (&&&) )
import Data.Aeson.Types
import Data.FileEmbed
import qualified Data.Map.Strict  as M
import qualified Data.Yaml        as YAML
import Instances.TH.Lift          ()
import Language.Haskell.TH.Syntax

import EP.Describable

embedYAML :: forall a. (Lift a, FromJSON a) => FilePath -> Q Exp
embedYAML path = lift 
             =<< YAML.decodeFileThrow @_ @a
             =<< makeRelativeToProject path

embedYAMLNameMap :: forall a. (Lift a, FromJSON a, Describable a) => FilePath -> Q Exp
embedYAMLNameMap path = lift . M.fromList . fmap (getName &&& id)
                    =<< YAML.decodeFileThrow @_ @[a]
                    =<< makeRelativeToProject path
