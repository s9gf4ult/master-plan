module MasterPlan.Input.Yaml.Interpretation where

import Control.Lens
import Data.Text as T
import Data.Yaml
import MasterPlan.Input.Yaml.Struct
import MasterPlan.Project as P

data ParsingOpts = ParsingOpts
  { _poRootKey     :: Maybe Text
  , _poParseStrict :: Bool
  } deriving (Eq, Ord)

makeLenses ''ParsingOpts

parseYamlModule :: FilePath -> IO (Either String Module)
parseYamlModule fp = over _Left prettyPrintParseException <$> decodeFileEither fp

-- | Load modules recursively or throw an error
loadYamlModules :: FilePath -> IO [Module]
loadYamlModules = error "Not implemented: loadYamlModules"

interpretModules :: [Module] -> ParsingOpts -> IO (P.Project ())
interpretModules = error "Not implemented: interpretModules"
