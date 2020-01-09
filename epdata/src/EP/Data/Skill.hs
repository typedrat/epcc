module EP.Data.Skill ( skills ) where

import Control.Arrow              ( (&&&) )
import qualified Data.Text        as T
import qualified Data.Map.Strict  as M

import EP.Describable
import EP.Schema.Skill
import EP.Utils

skills :: M.Map T.Text AnySkill
skills = M.fromList 
       . fmap (getName &&& id)
       $ $(embedYAML @[AnySkill] =<< makeRelativeToProject "data/skills.yml")
