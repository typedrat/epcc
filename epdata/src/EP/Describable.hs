module EP.Describable ( Describable(..) ) where

import qualified Data.Text as T

class Describable a where
    getName        :: a -> T.Text
    getDescription :: a -> T.Text
