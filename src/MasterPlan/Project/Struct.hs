module MasterPlan.Project.Struct where

import Control.Lens
import Data.Aeson
import Data.Scientific
import Data.Set as S
import Data.Text as T
import MasterPlan.Algebra

newtype Cost = Cost
  { unCost :: Scientific
  } deriving (Eq, Ord, Show, FromJSON)

newtype Trust = Trust
  { unTrust :: Scientific
  } deriving (Eq, Ord, Show, FromJSON)

newtype Progress = Progress
  { unProgress :: Scientific
  } deriving (Eq, Ord, Show, FromJSON)

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

-- | The plan of project execution.
data ProjectPlan a
  = DirectOrder [ProjectPlan a]
  -- ^ Subplans of 'DirectOrder' are transitively dependent. Meaning
  -- if we have @a -> b -> c@ then @a -> c@
  | AnyOrder (Set (ProjectPlan a))
  -- ^ All subplans of AnyOrder are independent. They may be executed
  -- sequentially in any order or even simultaneously
  | PlannedTask a
  deriving (Eq, Ord, Foldable)

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
