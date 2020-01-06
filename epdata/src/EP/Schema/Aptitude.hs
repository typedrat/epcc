{-# LANGUAGE TemplateHaskell #-}
module EP.Schema.Aptitude ( Aptitude(..), AptitudeSet(..) ) where

import Control.Applicative
import Control.Lens.At
import Control.Lens.Combinators ( view )
import Control.Lens.TH
import Data.Aeson.Types
import qualified Data.Text      as T
import GHC.Generics             ( Generic )
import Text.Read

import EP.Describable

data Aptitude = COG | COO | INT | REF | SAV | SOM | WIL
              deriving (Show, Read, Eq, Ord, Generic, Enum)

instance Describable Aptitude where
    getName COG = "Cognition"
    getName COO = "Coordination"
    getName INT = "Intuition"
    getName REF = "Reflexes"
    getName SAV = "Savvy"
    getName SOM = "Somatics"
    getName WIL = "Will"

    getDescription COG = "Cognition (COG) is your aptitude for problem solving, logical analysis, and understanding. It also includes memory and recall."
    getDescription COO = "Coordination (COO) is your skill at integrating the actions of different parts of your morph to produce smooth, successful movements. It includes manual dexterity, fine motor control, nimbleness, and balance."
    getDescription INT = "Intuition (INT) is your skill at following your gut instincts and evaluating on the fly. It includes physical awareness, cleverness, and cunning."
    getDescription REF = "Reflexes (REF) is your skill at acting quickly. This encompasses your reaction time, your gut level response, and your ability to think fast."
    getDescription SAV = "Savvy (SAV) is your mental adaptability, social intuition, and proficiency for interacting with others. It includes social awareness and manipulation."
    getDescription SOM = "Somatics (SOM) is your skill at pushing your morph to the best of its physical ability, including the fundamental utilization of the morph strength, endurance, and sustained positioning and motion."
    getDescription WIL = "Willpower (WIL) is your skill for self-control, your ability to command your own destiny."

instance FromJSON Aptitude
instance ToJSON Aptitude
instance ToJSONKey Aptitude

--

data AptitudeSet = AptitudeSet 
                 { _cogApt
                 , _cooApt
                 , _intApt
                 , _refApt
                 , _savApt
                 , _somApt
                 , _wilApt :: Word
                 }
                 deriving (Show, Eq)

instance Semigroup AptitudeSet where
    (AptitudeSet cog1 coo1 int1 ref1 sav1 som1 wil1) <> (AptitudeSet cog2 coo2 int2 ref2 sav2 som2 wil2) = AptitudeSet (cog1 + cog2) (coo1 + coo2) (int1 + int2) (ref1 + ref2) (sav1 + sav2) (som1 + som2) (wil1 + wil2)

instance Monoid AptitudeSet where
    mempty = AptitudeSet 0 0 0 0 0 0 0

makeLenses ''AptitudeSet

type instance Index AptitudeSet = Aptitude
type instance IxValue AptitudeSet = Word

instance Ixed AptitudeSet where
    ix COG = cogApt
    ix COO = cooApt
    ix INT = intApt
    ix REF = refApt
    ix SAV = savApt
    ix SOM = somApt
    ix WIL = wilApt

instance FromJSON AptitudeSet where
    parseJSON = withObject "AptitudeSet" $ \o -> AptitudeSet 
                                             <$> o .: "COG"
                                             <*> o .: "COO"
                                             <*> o .: "INT"
                                             <*> o .: "REF"
                                             <*> o .: "SAV"
                                             <*> o .: "SOM"
                                             <*> o .: "WIL"

aptitudeSetToJSONHelper :: (KeyValue kv) => AptitudeSet -> [kv]
aptitudeSetToJSONHelper as = [ "COG" .= view cogApt as
                             , "COO" .= view cooApt as
                             , "INT" .= view intApt as
                             , "REF" .= view refApt as
                             , "SAV" .= view savApt as
                             , "SOM" .= view somApt as
                             , "WIL" .= view wilApt as
                             ]

instance ToJSON AptitudeSet where
    toJSON = object . aptitudeSetToJSONHelper
    toEncoding = pairs . mconcat . aptitudeSetToJSONHelper
