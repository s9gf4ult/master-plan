module MasterPlan.Project where

import Control.Lens
import Data.Aeson
import Data.Scientific
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

data Composite = Composite
  { _cAlgebra     :: Algebra Project
  , _cAttributes  :: Attributes
  , _cCalculation :: Maybe Calculation
  } deriving (Eq, Ord)

data Task = Task
  { _tAttributes  :: Attributes
  , _tCalculation :: Calculation
  } deriving (Eq, Ord)

data Attributes = Attributes
  { _aTitle       :: Maybe Text
  , _aDescription :: Maybe Text
  , _aUrl         :: Maybe Text
  , _aOwner       :: Maybe Text
  } deriving (Eq, Ord)

data Calculation = Calculation
  { _cTrust    :: Trust
  , _cProgress :: Progress
  , _cCost     :: Cost
  } deriving (Eq, Ord)

data Project
  = PComposite Composite
  | PTask Task
  deriving (Eq, Ord)

makePrisms ''Project
makeLenses ''Composite
makeLenses ''Task
makeLenses ''Attributes
makeLenses ''Calculation
