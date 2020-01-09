{-# OPTIONS_GHC -Wno-orphans #-}
module EP.Schema
    ( module M
    ) where

import Data.Aeson.Types
import qualified Data.Map.Strict as Map
import qualified Data.Text       as T

import EP.Schema.Aptitude  as M
import EP.Schema.Modifier  as M
import EP.Schema.Predicate as M
import EP.Schema.Skill     as M
import EP.Schema.Source    as M
import EP.Schema.Trait     as M

import EP.Data.Skill

-- Orphan instance we can't define normally due to the TH stage restriction :(

instance FromJSONKey AnySkill where
    fromJSONKey = FromJSONKeyTextParser $ \key -> do
        case Map.lookup key skills of
            Just match -> pure match
            Nothing    -> fail ('\'' : T.unpack key ++ "' is not a known/valid skill.")
