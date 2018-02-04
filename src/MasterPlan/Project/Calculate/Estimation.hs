module MasterPlan.Project.Calculate.Estimation where

import Control.Monad.State.Strict
import Data.List as L
import Data.Set as S
import MasterPlan.Algebra
import MasterPlan.Internal.Import
import MasterPlan.Project.Calculate.Plan
import MasterPlan.Project.Calculate.Variants
import MasterPlan.Project.Graph
import MasterPlan.Project.Plan
import MasterPlan.Project.Struct

calculateProject :: Project () -> Variants (Project (Maybe Calculation))
calculateProject proj = do
  plan <- planProject proj
  return $ putProjectEstimation plan proj

estimatePlan :: ProjectPlan Task -> Estimation
estimatePlan = \case
  DirectOrderPlan dop -> estimateDirectOrder dop
  AnyOrderPlan aop -> estimateAnyOrder aop

estimateDirectOrder :: DirectOrder Task -> Estimation
estimateDirectOrder = \case
  PlannedTask t -> t ^. estimation
  DirectOrder aops ->
    let
      ests = estimateAnyOrder <$> aops
      estCount = genericLength ests
      res = Estimation
        { _estimationTrust    = productOf (traversed . trust) ests
        , _estimationProgress = sumOf (traversed . progress) ests / estCount
        , _estimationCost     = sumOf (traversed . cost) ests
        , _estimationSteps    = sumOf (traversed . steps) ests
        }
    in res

estimateAnyOrder :: AnyOrder Task -> Estimation
estimateAnyOrder (AnyOrder ords) =
  let
    ests = estimateDirectOrder <$> S.toList ords
    estCount = genericLength ests
    res = Estimation
      { _estimationTrust    = productOf (traversed . trust) ests
      , _estimationProgress = sumOf (traversed . progress) ests / estCount
      , _estimationCost     = sumOf (traversed . cost) ests
      , _estimationSteps    = maybe 1 (+1) (maximumOf (traversed . steps) ests)
      }
  in res

putProjectEstimation
  :: ProjectPlan Task
  -> Project ()
  -> Project (Maybe Calculation)
putProjectEstimation plan proj = case proj of
  PTask t      -> PTask t
  PComposite c ->
    let
      newAlg = putAlgebraEstimation plan $ c ^. algebra
      newPayload = do
        newPlan <- filterPlan (projectTasks proj) plan
        let est = estimatePlan newPlan
        return $ Calculation est newPlan
      res = c
        { _compositeAlgebra = newAlg
        , _compositePayload = newPayload }
    in PComposite res

putAlgebraEstimation
  :: ProjectPlan Task
  -> Algebra (Project ())
  -> Algebra (Project (Maybe Calculation))
putAlgebraEstimation plan alg = putProjectEstimation plan <$> alg

projectTasks :: Project a -> Set Task
projectTasks = \case
  PTask t      -> S.singleton t
  PComposite c -> S.unions $ c ^.. algebra . folded . to projectTasks

filterPlan :: Set Task -> ProjectPlan Task -> Maybe (ProjectPlan Task)
filterPlan tasks = \case
  DirectOrderPlan d -> DirectOrderPlan <$> filterDirectOrder tasks d
  AnyOrderPlan a    -> AnyOrderPlan <$> filterAnyOrder tasks a

filterDirectOrder :: Set Task -> DirectOrder Task -> Maybe (DirectOrder Task)
filterDirectOrder tasks = \case
  PlannedTask t
    | S.member t tasks -> Just $ PlannedTask t
    | otherwise        -> Nothing
  DirectOrder ords -> case catMaybes $ filterAnyOrder tasks <$> ords of
    [] -> Nothing
    x  -> Just $ DirectOrder x

filterAnyOrder :: Set Task -> AnyOrder Task -> Maybe (AnyOrder Task)
filterAnyOrder tasks = \case
  AnyOrder ords -> case catMaybes $ filterDirectOrder tasks <$> S.toList ords of
    [] -> Nothing
    x  -> Just $ AnyOrder $ S.fromList x
