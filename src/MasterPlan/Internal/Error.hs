-- | Any error can be thrown in master plan

module MasterPlan.Internal.Error where

import Control.Exception

data MasterPlanError
  = ParserError String
  deriving (Eq, Show)

instance Exception MasterPlanError
