{-# LANGUAGE TemplateHaskell #-}
module EP.Schema.Skill 
    ( SkillType(..), SkillCategory(..)
    , SkillData(..), HasSkillData(..)
    , Skill(..), AnySkill(..)
    , SkillRanks(..), HasSkillRanks(..), FieldSkillRanks
    ) where

import Control.Lens.At
import Control.Lens.Combinators  ( view, coerced )
import Control.Lens.Lens
import Control.Lens.TH
import Data.Aeson.Types
import Data.Functor.Contravariant
import qualified Data.Text       as T
import qualified Data.Map.Strict as M
import GHC.Generics              ( Generic )

import EP.Describable
import EP.Schema.Aptitude

data SkillType = Normal | Field

data SkillCategory = Active | Knowledge
                   deriving (Show, Eq, Ord, Generic)

instance FromJSON SkillCategory
instance ToJSON SkillCategory

data SkillData = SkillData
               { _sdName :: T.Text
               , _sdAptitude :: Aptitude
               , _sdCategory :: SkillCategory
               , _sdDescription :: T.Text
               , _sdDefaultable :: Bool
               }
               deriving (Show, Eq, Ord)

makeClassy ''SkillData

data Skill (t :: SkillType) where
    NormalSkill :: SkillData -> Skill 'Normal
    FieldSkill :: SkillData -> Skill 'Field

skillType :: Skill t -> SkillType
skillType NormalSkill{} = Normal
skillType FieldSkill{} = Field

deriving instance Show (Skill t)
deriving instance Eq (Skill t)
deriving instance Ord (Skill t)

instance HasSkillData (Skill t) where
    skillData = lens get set
        where
            get :: Skill t -> SkillData
            get (NormalSkill sd) = sd
            get (FieldSkill sd)  = sd

            set :: Skill t -> SkillData -> Skill t
            set NormalSkill{} sd = NormalSkill sd
            set FieldSkill{}  sd = FieldSkill sd
    {-# INLINE skillData #-}

instance Describable (Skill t) where
    getName = view sdName
    getDescription = view sdDescription

skillToJSONHelper :: (HasSkillData s, KeyValue kv) => (s -> SkillType) -> s -> [kv]
skillToJSONHelper getType skill = concat [reqs, def', field]
    where
        reqs = [ "name" .= name
               , "aptitude" .= apt
               , "category" .= cat
               , "description" .= desc
               ]

        field
            | Field <- getType skill = ["field_skill" .= True]
            | otherwise              = []

        def'
            | not def   = ["defaultable" .= False]
            | otherwise = []

        SkillData { _sdName = name
                  , _sdAptitude = apt
                  , _sdCategory = cat
                  , _sdDescription = desc
                  , _sdDefaultable = def
                  } = view skillData skill

instance ToJSON (Skill t) where
    toJSON = object . skillToJSONHelper skillType
    toEncoding = pairs . mconcat . skillToJSONHelper skillType

instance ToJSONKey (Skill t) where
    toJSONKey = contramap getName toJSONKey
    toJSONKeyList = contramap (fmap getName) toJSONKeyList

--

data AnySkill where
    AnySkill :: forall t. Skill t -> AnySkill

anySkillType :: AnySkill -> SkillType
anySkillType (AnySkill sk)  = skillType sk

deriving instance Show AnySkill

instance Eq AnySkill where
    (AnySkill x@NormalSkill{}) == (AnySkill y@NormalSkill{}) = x == y
    (AnySkill x@FieldSkill{})  == (AnySkill y@FieldSkill{})  = x == y
    _                          == _                          = False

instance Ord AnySkill where
    compare (AnySkill x@NormalSkill{}) (AnySkill y@NormalSkill{}) = compare x y
    compare (AnySkill x@FieldSkill{})  (AnySkill y@FieldSkill{})  = compare x y
    compare (AnySkill NormalSkill{})   (AnySkill FieldSkill{})    = LT
    compare (AnySkill FieldSkill{})    (AnySkill NormalSkill{})   = GT

instance HasSkillData AnySkill where
    skillData = lens get set
        where
            get :: AnySkill -> SkillData
            get (AnySkill (NormalSkill sd)) = sd
            get (AnySkill (FieldSkill sd))  = sd

            set :: AnySkill -> SkillData -> AnySkill
            set (AnySkill NormalSkill{}) sd = AnySkill (NormalSkill sd)
            set (AnySkill FieldSkill{})  sd = AnySkill (FieldSkill sd)
    {-# INLINE skillData #-}

instance Describable AnySkill where
    getName = view sdName
    getDescription = view sdDescription

instance FromJSON AnySkill where
    parseJSON = withObject "Skill" $ \o -> do
        name  <- o .:  "name"
        apt   <- o .:  "aptitude"
        cat   <- o .:  "category"
        desc  <- o .:  "description"
        def   <- o .:? "defaultable" .!= True
        field <- o .:? "field_skill" .!= False

        let sd = SkillData name apt cat desc def
            skill = if field then AnySkill (FieldSkill sd)
                             else AnySkill (NormalSkill sd)

        return skill

instance ToJSON AnySkill where
    toJSON = object . skillToJSONHelper anySkillType
    toEncoding = pairs . mconcat . skillToJSONHelper anySkillType

instance ToJSONKey AnySkill where
    toJSONKey = contramap getName toJSONKey
    toJSONKeyList = contramap (fmap getName) toJSONKeyList

--

data SkillRanks = SkillRanks { _srRanks :: Word, _srSpecializations :: [T.Text] }
                deriving (Show, Eq)

instance Semigroup SkillRanks where
    (SkillRanks r1 s1) <> (SkillRanks r2 s2) = SkillRanks (r1 + r2) (s1 ++ s2)

instance Monoid SkillRanks where
    mempty = SkillRanks 0 []

instance FromJSON SkillRanks where
    parseJSON = withObject "skill ranks" $ \o -> SkillRanks
                    <$> o .:  "ranks"
                    <*> o .:? "specializations" .!= []

instance ToJSON SkillRanks where
    toJSON (SkillRanks ranks specs) 
        | not (null specs) = object ["ranks" .= ranks, "specializations" .= specs]
        | otherwise        = object ["ranks" .= ranks]
    toEncoding (SkillRanks ranks specs)
        | not (null specs) = pairs ("ranks" .= ranks <> "specializations" .= specs)
        | otherwise        = pairs ("ranks" .= ranks)

makeClassy ''SkillRanks

newtype FieldSkillRanks = FieldSkillRanks { unFieldSkillRanks :: M.Map T.Text SkillRanks }
                        deriving (Show, Eq)

instance Semigroup FieldSkillRanks where
    (FieldSkillRanks r1) <> (FieldSkillRanks r2) = FieldSkillRanks (M.unionWith (<>) r1 r2)

instance Monoid FieldSkillRanks where
    mempty = FieldSkillRanks mempty

type instance Index FieldSkillRanks = T.Text
type instance IxValue FieldSkillRanks = SkillRanks

instance Ixed FieldSkillRanks
instance At FieldSkillRanks where
    at key = coerced . at @(M.Map T.Text SkillRanks) key
