module MasterPlan.Project.Calculate where

import Control.Lens
import Data.Set as S
import MasterPlan.Algebra
import MasterPlan.Project.Struct

calculateProject :: Project () -> Project Calculation
calculateProject = \case
  PTask a -> PTask a
  PComposite c -> PComposite $ calculateComposite c

calculateComposite :: Composite () -> Composite Calculation
calculateComposite c = (error "FIXME: ")


{-| Calculates plan of arbitrary project

Assumptions:

* Algebras of all projects are already flattened

* There is no cycles. Meaning if project A depends on B then B should
  not depend on A neither directly nor indirectly

-}

planProject
  :: Project a
  -- ^ Project tree to calculate
  -> [ProjectPlan Task]
  -- ^ List of possible plans.
planProject = \case
  PTask t      -> [PlannedTask t]
  PComposite c -> planAlgebra $ c ^. algebra

planAlgebra :: Algebra (Project a) -> [ProjectPlan Task]
planAlgebra = \case
  Sum ones -> ones >>= planAlgebra
  Product algs -> do
    plans <- traverse planAlgebra algs
    return $ AnyOrder $ S.fromList $ removeSubplans plans
  Sequence algs -> do
    plans <- traverse planAlgebra algs
    return $ DirectOrder $ cleanPlansSequence plans

-- | Remove plans which are subset of other plans
removeSubplans :: [ProjectPlan Task] -> [ProjectPlan Task]
removeSubplans = error "Not implemented: removeSubplans"

-- | Remove tasks from subsequent plans which are already executed
cleanPlansSequence :: [ProjectPlan Task] -> [ProjectPlan Task]
cleanPlansSequence = error "Not implemented: cleanPlansSequence"
