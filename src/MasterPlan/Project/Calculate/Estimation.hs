module MasterPlan.Project.Calculate.Estimation where

import Control.Monad.State.Strict
import Data.Set as S
import MasterPlan.Algebra
import MasterPlan.Internal.Import
import MasterPlan.Project.Calculate.Plan
import MasterPlan.Project.Calculate.Variants
import MasterPlan.Project.Graph
import MasterPlan.Project.Plan
import MasterPlan.Project.Struct

calculateProject :: Project () -> Variants (Project Calculation)
calculateProject proj = do
  plan <- planProject proj
  return $ putProjectEstimation plan proj

estimatePlan :: ProjectPlan Task -> Calculation
estimatePlan = error "Not implemented: estimatePlan"

putProjectEstimation :: ProjectPlan Task -> Project () -> Project Calculation
putProjectEstimation plan proj = case proj of
  PTask t -> PTask t
  PComposite c ->
    let newAlg = putAlgebraEstimation plan $ c ^. algebra
        newPayload = estimatePlan $ filterPlan (projectTasks proj) plan
        res = c
          { _compositeAlgebra = newAlg
          , _compositePayload = newPayload }
    in PComposite res

putAlgebraEstimation
  :: ProjectPlan Task
  -> Algebra (Project ())
  -> Algebra (Project Calculation)
putAlgebraEstimation plan alg = putProjectEstimation plan <$> alg

projectTasks :: Project a -> Set Task
projectTasks = \case
  PTask t      -> S.singleton t
  PComposite c -> S.unions $ c ^.. algebra . folded . to projectTasks

filterPlan :: Set Task -> ProjectPlan Task -> ProjectPlan Task
filterPlan tasks = \case
  DirectOrderPlan d -> DirectOrderPlan $ filterDirectOrder tasks d
  AnyOrderPlan a -> AnyOrderPlan $ filterAnyOrder tasks a

filterDirectOrder :: Set Task -> DirectOrder Task -> DirectOrder Task
filterDirectOrder = error "Not implemented: filterDirectOrder"

filterAnyOrder :: Set Task -> AnyOrder Task -> AnyOrder Task
filterAnyOrder = error "Not implemented: filterAnyOrder"
