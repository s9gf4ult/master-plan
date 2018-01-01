module MasterPlan.Input.Types where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Char
import Data.HashMap.Strict
import Data.Hashable
import Data.List as L
import Data.Scientific
import Data.Text as T
import MasterPlan.Algebra
import MasterPlan.Internal.TH

newtype ProjectName = ProjectName
  { unProjectName :: Text
  } deriving (Eq, Hashable)

instance FromJSON ProjectName where
  parseJSON = withText "Project name" parseProjectName

instance FromJSONKey ProjectName where
  fromJSONKey = FromJSONKeyTextParser parseProjectName

parseProjectName :: Text -> Parser ProjectName
parseProjectName t = do
  let ok = T.all isAlphaNum t
  unless ok $ fail "Project name must consist of alphanums"
  return $ ProjectName t

newtype ModuleName = ModuleName
  { unModuleName :: [Text]
  } deriving (Eq)

instance FromJSON ModuleName where
  parseJSON = withText "Dot separated module name" parseModuleName

parseModuleName :: Text -> Parser ModuleName
parseModuleName t = do
  let pn = T.splitOn "." $ T.strip t
      ok = L.all (T.all isAlphaNum) pn
  unless ok $ fail "Module name must be dont separated string of alphanums"
  return $ ModuleName pn

newtype Cost = Cost
  { unCost :: Scientific
  } deriving (Eq, FromJSON)

newtype Trust = Trust
  { unTrust :: Scientific
  } deriving (Eq, FromJSON)

data Expression = Expression (Algebra ProjectName)
  deriving (Eq)

instance FromJSON Expression where
  parseJSON = withText "Project expression" go
    where
      go = (error "FIXME: parse expression")



data ModuleImport = ModuleImport
  { _miModule  :: ModuleName
  , _miSynonym :: Maybe ModuleName
  } deriving (Eq)

instance FromJSON ModuleImport where
  parseJSON = withText "Module import" go
    where
      go t = case T.splitOn " as " t of
        [m] -> do
          moduleName <- parseModuleName m
          return $ ModuleImport moduleName Nothing
        [m, syn] -> do
          moduleName <- parseModuleName m
          synonym <- parseModuleName syn
          return $ ModuleImport moduleName $ Just synonym
        _ -> fail "Unexpected count of \"as\" keywords in import"

data Project = Project
  { _pTitle       :: Maybe Text
  , _pDescription :: Maybe Text
  , _pUrl         :: Maybe Text
  , _pOwner       :: Maybe Text
  , _pCost        :: Maybe Cost
  , _pTrust       :: Maybe Trust
  , _pExpression  :: Maybe Expression
  } deriving (Eq)

makeLenses ''Project

deriveFromJSON jsonOpts ''Project

data Module = Module
  { _mName     :: ModuleName
  , _mImports  :: Maybe [ModuleImport]
  , _mRoot     :: Maybe ProjectName
  , _mProjects :: HashMap ProjectName Project
  } deriving (Eq)

makeLenses ''Module

deriveFromJSON jsonOpts ''Module
