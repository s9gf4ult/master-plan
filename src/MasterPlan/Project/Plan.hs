module MasterPlan.Project.Plan where

import Data.List as L
import Data.Set as S
import MasterPlan.Internal.Import

-- | The plan of project execution.
data ProjectPlan a
  = DirectOrderPlan (DirectOrder a)
  | AnyOrderPlan (AnyOrder a)
  deriving (Eq, Ord, Foldable)

projectPlanSingular :: ProjectPlan a -> Maybe a
projectPlanSingular = \case
  DirectOrderPlan p -> directOrderSingular p
  AnyOrderPlan p    -> anyOrderSingular p

directOrderSingular :: DirectOrder a -> Maybe a
directOrderSingular = \case
  DirectOrder [ao] -> anyOrderSingular ao
  DirectOrder _    -> Nothing
  PlannedTask a    -> Just a

anyOrderSingular :: AnyOrder a -> Maybe a
anyOrderSingular (AnyOrder (S.toList -> [direct])) = directOrderSingular direct
anyOrderSingular _ = Nothing

instance Pointed ProjectPlan where
  point a = DirectOrderPlan $ point a

newtype AnyOrder a
  = AnyOrder (Set (DirectOrder a))
  deriving (Eq, Ord, Foldable)

anyOrderPoint :: (Ord a) => a -> AnyOrder a
anyOrderPoint a = AnyOrder $ S.fromList [point a]

anyOrder :: (Ord a) => [ProjectPlan a] -> AnyOrder a
anyOrder [projectPlanSingular -> Just a] = anyOrderPoint a
anyOrder plans = AnyOrder $ S.unions $ toDirectOrder <$> plans
  where
    toDirectOrder = \case
      DirectOrderPlan p         -> S.singleton p
      AnyOrderPlan (AnyOrder p) -> p

data DirectOrder a
  = DirectOrder [AnyOrder a]
  | PlannedTask a
  deriving (Eq, Ord, Foldable)

instance Pointed DirectOrder where
  point a = PlannedTask a

directOrderLength :: DirectOrder a -> Int
directOrderLength = \case
  DirectOrder a -> L.length a
  PlannedTask _ -> 1

directOrder :: (Ord a) => [ProjectPlan a] -> DirectOrder a
directOrder [projectPlanSingular -> Just a] = PlannedTask a
directOrder plans = DirectOrder $ plans >>= toAnyOrders
  where
    toAnyOrders = \case
      DirectOrderPlan p -> case p of
        DirectOrder a -> a
        PlannedTask a -> [anyOrderPoint a]
      AnyOrderPlan a -> [a]
