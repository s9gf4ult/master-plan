module MasterPlan.Input.Json where

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
import MasterPlan.Input.Expression
import MasterPlan.Internal.TH

newtype Cost = Cost
  { unCost :: Scientific
  } deriving (Eq, Show, FromJSON)

newtype Trust = Trust
  { unTrust :: Scientific
  } deriving (Eq, Show, FromJSON)

data Project = Project
  { _pTitle       :: Maybe Text
  , _pDescription :: Maybe Text
  , _pUrl         :: Maybe Text
  , _pOwner       :: Maybe Text
  , _pCost        :: Maybe Cost
  , _pTrust       :: Maybe Trust
  , _pExpression  :: Maybe Expression
  } deriving (Eq, Show)

makeLenses ''Project

deriveFromJSON jsonOpts ''Project

data Module = Module
  { _mName     :: ModuleName
  , _mImports  :: Maybe [ModuleImport]
  , _mRoot     :: Maybe ProjectName
  , _mProjects :: HashMap ProjectName Project
  } deriving (Eq, Show)

makeLenses ''Module

deriveFromJSON jsonOpts ''Module
