module EP.Data.Skill ( skills ) where

import qualified Data.Text       as T
import qualified Data.Map.Strict as M

import EP.Schema.Skill
import EP.Utils

skills :: M.Map T.Text AnySkill
skills = $(embedYAMLNameMap @AnySkill "data/skills.yml")
