module MasterPlan.Input.Yaml.Struct where

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.HashMap.Strict
import Data.Text as T
import Data.Yaml
import MasterPlan.Input.Yaml.Expression
import MasterPlan.Internal.TH

import qualified MasterPlan.Project as P

data Project = Project
  { _pTitle       :: Maybe Text
  , _pDescription :: Maybe Text
  , _pUrl         :: Maybe Text
  , _pOwner       :: Maybe Text
  , _pCost        :: Maybe P.Cost
  , _pTrust       :: Maybe P.Trust
  , _pProgress    :: Maybe P.Progress
  , _pExpression  :: Maybe Expression
  } deriving (Eq, Show)

makeLenses ''Project

instance FromJSON Project where
  parseJSON v = case v of
    Object o -> do
      title <- o .:?  "title"
      desc  <- o .:? "description" >>= \case
        Nothing -> o .:? "desc"
        a       -> return a
      url   <- o .:? "url"
      owner <- o .:? "owner"
      cost  <- o .:? "cost"
      trust <- o .:? "trust"
      prog <- o .:? "progress " >>= \case
        Nothing -> o .:? "prog"
        a       -> return a
      expr  <- o .:? "expression" >>= \case
        Nothing -> o .:? "expr"
        a       -> return a
      return Project
        { _pTitle       = title
        , _pDescription = desc
        , _pUrl         = url
        , _pOwner       = owner
        , _pCost        = cost
        , _pTrust       = trust
        , _pProgress    = prog
        , _pExpression  = expr }
    String t -> do
      expr <- parseExpression t
      return $ emptyProject { _pExpression = Just expr }
    _ -> fail "Project must be either string or object"

emptyProject :: Project
emptyProject = Project
  { _pTitle       = Nothing
  , _pDescription = Nothing
  , _pUrl         = Nothing
  , _pOwner       = Nothing
  , _pCost        = Nothing
  , _pTrust       = Nothing
  , _pProgress    = Nothing
  , _pExpression  = Nothing
  }

data Module = Module
  { _mModule   :: ModuleName
  , _mImports  :: Maybe [ModuleImport]
  , _mRoot     :: Maybe ProjectName
  , _mProjects :: HashMap ProjectName Project
  } deriving (Eq, Show)

makeLenses ''Module

deriveFromJSON jsonOpts ''Module
