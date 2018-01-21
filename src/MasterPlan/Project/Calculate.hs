module MasterPlan.Project.Calculate where

import Control.Arrow
import Control.Lens
import Control.Monad.State.Strict
import Data.Foldable as F
import Data.List as L
import Data.Map.Strict as M
import Data.Set as S
import MasterPlan.Algebra
import MasterPlan.Internal.Import
import MasterPlan.Project.Graph
import MasterPlan.Project.Plan
import MasterPlan.Project.Struct

calculateProject :: Project () -> Project Calculation
calculateProject = \case
  PTask a -> PTask a
  PComposite c -> PComposite $ calculateComposite c

calculateComposite :: Composite () -> Composite Calculation
calculateComposite c = (error "FIXME: ")

-- | Newtype over list to distinguish just list of plans from list of
-- independent plans, or in other words list of plan variants.
newtype Variants a = Variants
  { unVariants :: [a]
  } deriving (Eq, Monoid, Functor, Foldable, Applicative, Monad)

variants :: [a] -> Variants a
variants = Variants

{-| Calculates plan of arbitrary project

Assumptions:

* Algebras of all projects are already flattened

* There is no cycles. Meaning if project A depends on B then B should
  not depend on A neither directly nor indirectly

-}

planProject
  :: (Ord a)
  => Project a
  -- ^ Project tree to calculate
  -> Variants (ProjectPlan Task)
  -- ^ List of possible plans.
planProject proj = do
  dirtyPlan <- dirtyPlanProject proj
  deintersectPlan $ cleanPlanSequence dirtyPlan

dirtyPlanProject :: Project a -> Variants (ProjectPlan Task)
dirtyPlanProject = \case
  PTask t      -> variants [point t]
  PComposite c -> dirtyAlgebraPlan $ c ^. algebra

dirtyAlgebraPlan :: Algebra (Project a) -> Variants (ProjectPlan Task)
dirtyAlgebraPlan = \case
  Sum algs     -> variants algs >>= dirtyAlgebraPlan
  Product algs -> do
    subplans <- traverse dirtyAlgebraPlan algs
    return $ DirectOrderPlan $ directOrder subplans
  Sequence algs -> do
    subplans <- traverse dirtyAlgebraPlan algs
    return $ AnyOrderPlan $ anyOrder subplans
  Atom proj -> dirtyPlanProject proj

-- | Remove tasks from subsequent plans which are already executed
cleanPlanSequence :: ProjectPlan Task -> ProjectPlan Task
cleanPlanSequence plan = flip evalState S.empty go
  where
    go = case plan of
      DirectOrderPlan p -> DirectOrderPlan <$> goDirectOrder p
      AnyOrderPlan p    -> AnyOrderPlan <$> goAnyOrder p
    goDirectOrder :: DirectOrder Task -> State (Set (ProjectPlan Task)) (DirectOrder Task)
    goDirectOrder d = case d of
      DirectOrder d -> do
        maos <- for d $ \ao -> do
          s <- get
          let aop = AnyOrderPlan ao
          case S.member aop s of
            True -> return Nothing
            False -> do
              put $ S.insert aop s
              res <- goAnyOrder ao
              return $ Just res
        let
          res = case catMaybes maos of
            [anyOrderSingular -> Just t] -> point t
            x                            -> DirectOrder x
        return res
      a@(PlannedTask {}) -> return a
      -- This case will be checked in 'goAnyOrder'
    goAnyOrder :: AnyOrder Task -> State (Set (ProjectPlan Task)) (AnyOrder Task)
    goAnyOrder (AnyOrder (S.toList -> directs)) = do
      s <- get
      let
        aoProcessed = catMaybes $ flip fmap directs $ \dir ->
          let p = DirectOrderPlan dir
          in case S.member p s of
            True  -> Nothing
            False ->
              let (resDir, resS) = runState (goDirectOrder dir) s
              in Just (resDir, S.insert p resS)
        finalS = S.unions $ snd <$> aoProcessed
        res = case fst <$> aoProcessed of
          [directOrderSingular -> Just t] -> anyOrderPoint t
          x -> AnyOrder $ S.fromList x
      put finalS
      return res

deintersectPlan :: ProjectPlan a -> Variants (ProjectPlan a)
deintersectPlan = \case
  DirectOrderPlan direct -> DirectOrderPlan <$> deintersectSequence direct
  AnyOrderPlan ao -> AnyOrderPlan <$> deintersectProduct ao

deintersectSequence :: DirectOrder a -> Variants (DirectOrder a)
deintersectSequence = \case
  DirectOrder aos -> DirectOrder <$> traverse deintersectProduct aos
  t@(PlannedTask {}) -> return t

deintersectProduct :: AnyOrder a -> Variants (AnyOrder a)
deintersectProduct (AnyOrder directs) = (error "FIXME: ")


-- planAlgebra
--   :: (Ord a)
--   => Algebra (Project a)
--   -> Variants (ProjectPlan Task)
-- planAlgebra = \case
--   Sum ones      -> variants ones >>= planAlgebra
--   Product algs  -> planProduct algs
--   Sequence algs -> DirectOrderPlan <$> planSequence algs
--   Atom p        -> planProject p

-- planSequence
--   :: (Ord a)
--   => [Algebra (Project a)]
--   -> Variants (DirectOrder Task)
-- planSequence algs = do
--   seqPlan <- traverse planAlgebra algs
--   return $ directOrder $ cleanPlanSequence seqPlan

-- planProduct
--   :: forall a. (Ord a)
--   => [Algebra (Project a)]
--   -> Variants (ProjectPlan Task)
-- planProduct algs = do
--   let
--     connected :: [ConnectedComponent (Algebra (Project a))]
--     connected = connectedComponents algebraConnected algs
--   case connected of
--     [single] -> connectedAlgebrasPlan single
--     _        -> do
--       independents <- traverse connectedAlgebrasPlan connected
--       return $ AnyOrderPlan $ anyOrder independents

-- algebraConnected
--   :: (Ord a)
--   => Algebra (Project a)
--   -> Algebra (Project a)
--   -> Bool
-- algebraConnected a b = not $ S.null
--   $ S.intersection (S.fromList $ allSubprojects a) (S.fromList $ allSubprojects b)

-- allSubprojects :: Algebra (Project a) -> [Project a]
-- allSubprojects a = do
--   proj <- F.toList a
--   let nestedProjs = (proj ^.. _PComposite . algebra) >>= allSubprojects
--   proj : nestedProjs

-- -- | Plan algebras having common projects. All algebras are depend
-- -- with each other. Meaning they form non-strongly connected graph
-- -- where each vertex is an algebra and each edge is the fact that two
-- -- algebras have common projects
-- connectedAlgebrasPlan
--   :: (Ord a)
--   => ConnectedComponent (Algebra (Project a))
--   -> Variants (ProjectPlan Task)
-- connectedAlgebrasPlan algs =
--   let
--     -- Plans grouped by length. Current heuristic is to take shortest
--     -- plans from whole list to minimize computational overhead
--     plansLenMap :: Map Int (Variants (DirectOrder Task))
--     plansLenMap
--       = M.fromListWith (<>)
--       $ unVariants
--       $ fmap (directOrderLength &&& pure)
--       $ possibleDirectPlans algs
--     directPlans = case M.toAscList plansLenMap of
--       []             -> mempty
--       ((_, plans):_) -> plans
--   in DirectOrderPlan <$> directPlans

-- possibleDirectPlans
--   :: forall a. (Ord a)
--   => ConnectedComponent (Algebra (Project a))
--   -- ^ Algebras having common projects in formula
--   -> Variants (DirectOrder Task)
--   -- ^ Variants of sequences. Each list in variant is a payload for
--   -- 'DirectOrder' constructor
-- possibleDirectPlans algs = do
--   let
--     conMap :: Map Int [Algebra (Project a)]
--     conMap = M.fromListWith (++)
--       $ fmap (snd &&& ((:[]) . fst))
--       $ M.toList
--       $ edgesCount algs
--     mostConnected = case M.toDescList conMap of
--       []         -> mempty
--       ((_, a):_) -> a
--   cuttedNode <- variants mostConnected
--   headPlan <- planAlgebra cuttedNode
--   let independents = removeNode cuttedNode algs
--   tailPlans <- traverse possibleDirectPlans independents
--   return $ directOrder
--     [ headPlan
--     , AnyOrderPlan $ AnyOrder $ S.fromList tailPlans ]
