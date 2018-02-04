module MasterPlan.Project.Struct where

import Data.Aeson
import Data.Scientific
import Data.Text as T
import MasterPlan.Algebra
import MasterPlan.Internal.Import
import MasterPlan.Project.Plan

newtype Cost = Cost
  { unCost :: Scientific
  } deriving (Eq, Ord, Show, Num, FromJSON)

newtype Trust = Trust
  { unTrust :: Scientific
  } deriving (Eq, Ord, Show, Num, FromJSON)

newtype Progress = Progress
  { unProgress :: Scientific
  } deriving (Eq, Ord, Show, Num, Fractional, FromJSON)

-- | Steps count to execute plan
newtype Steps = Steps
  { unSteps :: Int
  } deriving (Eq, Ord, Num)

-- | To distinguish same looking projects from each other
data ProjectId = ProjectId
  { _projectIdModuleName  :: [Text]
  , _projectIdProjectName :: Text
  } deriving (Eq, Ord)

data Composite a = Composite
  { _compositeId         :: ProjectId
  , _compositeAlgebra    :: Algebra (Project a)
  , _compositeAttributes :: Attributes
  , _compositePayload    :: a
  } deriving (Eq, Ord)

-- | Payload for 'Project'
data Calculation = Calculation
  { _calculationEstimation :: Estimation
  , _calculationPlan       :: ProjectPlan Task
  } deriving (Eq, Ord)

data Task = Task
  { _taskId         :: ProjectId
  , _taskAttributes :: Attributes
  , _taskEstimation :: Estimation
  } deriving (Eq, Ord)

data Attributes = Attributes
  { _attributesTitle       :: Maybe Text
  , _attributesDescription :: Maybe Text
  , _attributesUrl         :: Maybe Text
  , _attributesOwner       :: Maybe Text
  } deriving (Eq, Ord)

data Estimation = Estimation
  { _estimationTrust    :: Trust
  , _estimationProgress :: Progress
  , _estimationCost     :: Cost
  , _estimationSteps    :: Steps
  } deriving (Eq, Ord)

data Project a
  = PComposite (Composite a)
  | PTask Task
  deriving (Eq, Ord)

makePrisms ''Project
makeFields ''Composite
makeFields ''Task
makeFields ''Attributes
makeFields ''Estimation
makeFields ''Calculation
makeFields ''ProjectId
