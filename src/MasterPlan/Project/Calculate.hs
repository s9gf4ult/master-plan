module MasterPlan.Project.Calculate where

import Control.Arrow
import Control.Lens
import Data.Map.Strict as M
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
  Product algs -> [planProduct algs]
  Sequence algs -> do
    plans <- traverse planAlgebra algs
    return $ DirectOrder $ cleanPlansSequence plans
  Atom p -> planProject p

planProduct :: forall a. [Algebra (Project a)] -> ProjectPlan Task
planProduct algs =
  let
    connected :: [[Algebra (Project a)]]
    connected = error "FIXME: list of algebras having common projects"
    res = case connected of
      [single] -> connectedAlgebrasPlan single
      _        -> AnyOrder $ S.fromList $ connectedAlgebrasPlan <$> connected
  in res

-- | Plan algebras having common projects. All algebras are depend
-- with each other. Meaning they form non-strongly connected graph
-- where each vertex is an algebra and each edge is the fact that two
-- algebras have common projects
connectedAlgebrasPlan :: [Algebra (Project a)] -> [ProjectPlan Task]
connectedAlgebrasPlan algs =
  let
    possibleDirectPlans :: [[ProjectPlan Task]]
    possibleDirectPlans = error "FIXME: cut off plan for all different ways"
    -- Choose shortest direct plans
    directPlans = M.toAscList $ M.fromListWith (++) $ fmap (length &&& (:[])) possibleDirectPlans
  in DirectOrder <$> directPlans

-- | Remove tasks from subsequent plans which are already executed
cleanPlansSequence :: [ProjectPlan Task] -> [ProjectPlan Task]
cleanPlansSequence = error "Not implemented: cleanPlansSequence"
