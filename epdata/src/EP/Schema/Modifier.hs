module EP.Schema.Modifier ( Modifier(..), SkillMod(..) ) where

import Control.Applicative
import Data.Aeson.Types           hiding ( parseField )
import qualified Data.Text        as T
import qualified Data.Map.Strict  as M
import GHC.Generics               ( Generic )
import Language.Haskell.TH.Syntax ( Lift )

import EP.Data.Skill
import EP.Describable
import EP.Schema.Aptitude
import EP.Schema.Skill

modOptions :: Options
modOptions = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3
                            , constructorTagModifier = \s -> camelTo2 '_' . take (length s - 3) $ s
                            , sumEncoding = ObjectWithSingleField
                            }

data Modifier = AptitudeMod { _amAptitude :: Aptitude, _amValue :: Word }
              | FlexibleAptitudeMod { _faAptitude :: Maybe Aptitude, _faValue :: Word }
              | SkillMod SkillMod
              deriving (Show, Eq, Ord, Generic, Lift)

instance FromJSON Modifier where
    parseJSON = genericParseJSON modOptions
instance ToJSON Modifier where
    toJSON = genericToJSON modOptions
    toEncoding = genericToEncoding modOptions

data SkillMod = NormalSkillMod (Skill 'Normal) Word
              | FieldSkillMod (Skill 'Field) T.Text Word
              | FlexibleNormalSkillMod (Maybe (Skill 'Normal)) Word
              | FlexibleFieldSkillMod (Skill 'Field) (Maybe T.Text) Word
              deriving (Show, Eq, Ord, Generic, Lift)

instance FromJSON SkillMod where
    parseJSON = withObject "SkillMod" parseSkillMod
        where
            parseSkillMod o = parseNormal o <|> parseField o 
                          <|> parseFlexNormal o <|> parseFlexField o
        
            parseNormal, parseField, parseFlexNormal, parseFlexField :: Object -> Parser SkillMod
            parseNormal o = do
                ns <- o .: "normal"
                flip (withObject "NormalSkillMod") ns $ \o' -> do
                    skillName <- o' .: "skill"
                    ranks <- o' .: "ranks" :: Parser Word

                    case M.lookup skillName skills of
                        Just (AnySkill skill@NormalSkill{}) -> pure (NormalSkillMod skill ranks)
                        Just _                              -> fail ('\'' : T.unpack skillName ++ "' is a field skill.")
                        Nothing                             -> fail ('\'' : T.unpack skillName ++ "' is not a known skill.")

            parseField o = do
                fs <- o .: "field"
                flip (withObject "FieldSkillMod") fs $ \o' -> do
                    skillName <- o' .: "skill"
                    field <- o' .: "field" :: Parser T.Text
                    ranks <- o' .: "ranks" :: Parser Word

                    case M.lookup skillName skills of
                        Just (AnySkill skill@FieldSkill{}) -> pure (FieldSkillMod skill field ranks)
                        Just (AnySkill NormalSkill{})      -> fail ('\'' : T.unpack skillName ++ "' is a normal skill.")
                        _                                  -> fail ('\'' : T.unpack skillName ++ "' is not a known skill.")

            parseFlexNormal o = do
                ns <- o .: "flexible_normal"
                flip (withObject "FlexibleNormalSkillMod") ns $ \o' -> do
                    mSkillName <- o' .:? "skill"
                    ranks <- o' .: "ranks" :: Parser Word

                    case mSkillName of
                        Just skillName -> case M.lookup skillName skills of
                            Just (AnySkill skill@NormalSkill{}) -> pure (FlexibleNormalSkillMod (Just skill) ranks)
                            Just _                              -> fail ('\'' : T.unpack skillName ++ "' is a field skill.")
                            _                                   -> fail ('\'' : T.unpack skillName ++ "' is not a known skill.")
                        Nothing -> pure (FlexibleNormalSkillMod Nothing ranks)

            parseFlexField o = do
                fs <- o .: "flexible_field"
                flip (withObject "FlexibleFieldSkillMod") fs $ \o' -> do
                    skillName <- o' .: "skill"
                    field <- o' .:? "field" :: Parser (Maybe T.Text)
                    ranks <- o' .: "ranks" :: Parser Word

                    case M.lookup skillName skills of
                        Just (AnySkill skill@FieldSkill{}) -> pure (FlexibleFieldSkillMod skill field ranks)
                        Just (AnySkill NormalSkill{})      -> fail ('\'' : T.unpack skillName ++ "' is a normal skill.")
                        _                                  -> fail ('\'' : T.unpack skillName ++ "' is not a known skill.")

skillModToJSONHelper :: (KeyValue kv) => SkillMod -> kv
skillModToJSONHelper (NormalSkillMod skill ranks) =
    "normal" .= object ["skill" .= getName skill, "ranks" .= ranks]
skillModToJSONHelper (FieldSkillMod skill field ranks) =
    "field" .= object ["skill" .= getName skill, "field" .= field, "ranks" .= ranks]
skillModToJSONHelper (FlexibleNormalSkillMod (Just skill) ranks) =
    "flexible_normal" .= object ["skill" .= getName skill, "ranks" .= ranks]
skillModToJSONHelper (FlexibleNormalSkillMod Nothing ranks) =
    "flexible_normal" .= object ["ranks" .= ranks]
skillModToJSONHelper (FlexibleFieldSkillMod skill (Just field) ranks) =
    "flexible_field" .= object ["skill" .= getName skill, "field" .= field, "ranks" .= ranks]
skillModToJSONHelper (FlexibleFieldSkillMod skill Nothing ranks) =
    "flexible_field" .= object ["skill" .= getName skill, "ranks" .= ranks]

instance ToJSON SkillMod where
    toJSON = object . pure . skillModToJSONHelper
    toEncoding = pairs . skillModToJSONHelper
