module EP.Data.Trait ( egoTraits, morphTraits ) where

import EP.Schema.Trait
import EP.Utils

egoTraits :: [EgoTrait]
egoTraits = $(embedYAML @[EgoTrait] =<< makeRelativeToProject "data/ego_traits.yml")

morphTraits :: [MorphTrait]
morphTraits = $(embedYAML @[MorphTrait] =<< makeRelativeToProject "data/morph_traits.yml")
