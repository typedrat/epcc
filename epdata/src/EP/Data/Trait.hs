module EP.Data.Trait ( egoTraits, morphTraits ) where

import qualified Data.Text       as T
import qualified Data.Map.Strict as M

import EP.Schema.Trait
import EP.Utils

egoTraits :: M.Map T.Text EgoTrait
egoTraits = $(embedYAMLNameMap @EgoTrait "data/ego_traits.yml")

morphTraits :: M.Map T.Text MorphTrait
morphTraits = $(embedYAMLNameMap @MorphTrait "data/morph_traits.yml")
