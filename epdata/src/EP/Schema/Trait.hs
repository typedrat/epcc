{-# LANGUAGE TemplateHaskell #-}
module EP.Schema.Trait 
    ( TraitCategory(..)
    , TraitData(..), HasTraitData(..)
    , EgoTrait, MorphTrait
    ) where

import Control.Lens               hiding ( (.=) )
import Data.Aeson.Types
import Data.Char                  ( toLower )
import qualified Data.Text        as T
import GHC.Generics               ( Generic )
import Language.Haskell.TH.Syntax ( Lift )

import EP.Describable
import EP.Schema.Modifier
import EP.Schema.Predicate
import EP.Schema.Source

--

data TraitType = Ego | Morph
               deriving (Show, Eq, Ord, Generic)

traitTypeOptions :: Options
traitTypeOptions = defaultOptions { constructorTagModifier = fmap toLower }

instance FromJSON TraitType where
    parseJSON = genericParseJSON traitTypeOptions

instance ToJSON TraitType where
    toJSON = genericToJSON traitTypeOptions
    toEncoding = genericToEncoding traitTypeOptions

--

data TraitCategory = PositiveTrait | NegativeTrait
                   deriving (Show, Eq, Ord, Generic, Lift)

traitCategoryOptions :: Options
traitCategoryOptions = defaultOptions { constructorTagModifier = tagModifier }
    where
        tagModifier s = fmap toLower (take (length s - 5) s)

instance FromJSON TraitCategory where
    parseJSON = genericParseJSON traitCategoryOptions

instance ToJSON TraitCategory where
    toJSON = genericToJSON traitCategoryOptions
    toEncoding = genericToEncoding traitCategoryOptions

--

data TraitData = TraitData
               { _tdName :: T.Text
               , _tdSource :: Source
               , _tdDescription :: T.Text
               , _tdCost :: Int
               , _tdCategory :: TraitCategory
               , _tdModifiers :: [Modifier]
               , _tdRequirements :: [Predicate]
               }
               deriving (Show, Eq, Ord, Generic, Lift)

parseTraitData :: Object -> Parser TraitData
parseTraitData o = TraitData 
                <$> o .:  "name"
                <*> o .:  "source"
                <*> o .:  "description"
                <*> o .:  "cost"
                <*> o .:  "category"
                <*> o .:? "modifiers"    .!= []
                <*> o .:? "requirements" .!= []

makeClassy ''TraitData

instance HasSource TraitData where
    source = tdSource

--

newtype EgoTrait = EgoTrait TraitData
                 deriving (Show, Eq, Ord, Lift)

newtype MorphTrait = MorphTrait TraitData
                   deriving (Show, Eq, Ord, Lift)

instance HasTraitData EgoTrait where
    traitData = coerced

instance HasSource EgoTrait where
    source = tdSource

instance HasTraitData MorphTrait where
    traitData = coerced

instance HasSource MorphTrait where
    source = tdSource

instance Describable EgoTrait where
    getName = view tdName
    getDescription = view tdDescription

instance Describable MorphTrait where
    getName = view tdName
    getDescription = view tdDescription

instance FromJSON EgoTrait where
    parseJSON = withObject "ego trait" $ \o -> do
        ty <- o .: "type"
        td <- parseTraitData o

        case ty of
            Ego   -> pure (EgoTrait td)
            Morph -> fail ('\'' : T.unpack (view tdName td) ++ "' is not an ego trait!")

instance FromJSON MorphTrait where
    parseJSON = withObject "morph trait" $ \o -> do
        ty <- o .: "type"
        td <- parseTraitData o

        case ty of
            Morph -> pure (MorphTrait td)
            Ego   -> fail ('\'' : T.unpack (view tdName td) ++ "' is not a morph trait!")

traitToJSONHelper :: (HasTraitData t, KeyValue kv) => TraitType -> t -> [kv]
traitToJSONHelper ty trait = concat [required, modsL, reqsL]
    where
        required = [ "name" .= name
                   , "source" .= src
                   , "type" .= ty
                   , "description" .= desc
                   , "cost" .= cost
                   , "category" .= cat
                   ]
        modsL
            | not (null mods) = [ "modifiers" .= mods ]
            | otherwise       = []
        reqsL
            | not (null reqs) = [ "requirements" .= reqs ] 
            | otherwise       = []

        name = trait ^. tdName
        src  = trait ^. tdSource
        desc = trait ^. tdDescription
        cost = trait ^. tdCost
        cat  = trait ^. tdCategory
        mods = trait ^. tdModifiers
        reqs = trait ^. tdRequirements

instance ToJSON EgoTrait where
    toJSON = object . traitToJSONHelper Ego
    toEncoding = pairs . mconcat . traitToJSONHelper Ego

instance ToJSON MorphTrait where
    toJSON = object . traitToJSONHelper Morph
    toEncoding = pairs . mconcat . traitToJSONHelper Morph
