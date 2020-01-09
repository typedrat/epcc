module EP.Utils ( handleYAMLParseErrors ) where

import qualified Data.Yaml as YAML

handleYAMLParseErrors :: Either YAML.ParseException a -> a
handleYAMLParseErrors (Right skills) = skills
handleYAMLParseErrors (Left err)     = error (YAML.prettyPrintParseException err)
