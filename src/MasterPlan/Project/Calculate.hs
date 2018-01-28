module MasterPlan.Project.Calculate where

import Control.Monad.State.Strict
import Data.Foldable as F
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

type CleanState t a = State (Set (ProjectPlan t)) a

-- | Remove tasks from subsequent plans which are already executed
cleanPlanSequence :: (Ord t) => ProjectPlan t -> ProjectPlan t
cleanPlanSequence plan = evalState go S.empty
  where
    go = case plan of
      DirectOrderPlan p -> DirectOrderPlan <$> cleanDirectOrder p
      AnyOrderPlan p    -> AnyOrderPlan    <$> cleanAnyOrder p

cleanDirectOrder :: (Ord t) => DirectOrder t -> CleanState t (DirectOrder t)
cleanDirectOrder d = case d of
  DirectOrder d -> do
    maos <- for d $ \ao -> do
      s <- get
      let aop = AnyOrderPlan ao
      case S.member aop s of
        True -> return Nothing
        False -> do
          put $ S.insert aop s
          res <- cleanAnyOrder ao
          return $ Just res
    let
      res = case catMaybes maos of
        [anyOrderSingular -> Just t] -> point t
        x                            -> DirectOrder x
    return res
  a@(PlannedTask {}) -> return a
  -- This case will be checked in 'cleanAnyOrder'

cleanAnyOrder :: (Ord t) => AnyOrder t -> CleanState t (AnyOrder t)
cleanAnyOrder (AnyOrder (S.toList -> directs)) = do
  s <- get
  let
    aoProcessed = catMaybes $ flip fmap directs $ \dir ->
      let p = DirectOrderPlan dir
      in case S.member p s of
        True  -> Nothing
        False ->
          let (resDir, resS) = runState (cleanDirectOrder dir) s
          in Just (resDir, S.insert p resS)
    finalS = S.unions $ snd <$> aoProcessed
    res = case fst <$> aoProcessed of
      [directOrderSingular -> Just t] -> anyOrderPoint t
      x                               -> AnyOrder $ S.fromList x
  put finalS
  return res

-- | Cleans subsequent connected component from tasks that already in
-- first direct order plan.
cleanSequentialGraph
  :: (Ord a)
  => DirectOrder a
  -- ^ Plan to take tasks from
  -> ConnectedComponent (DirectOrder a) (Set a)
  -- ^ Components to clean in
  -> [ConnectedComponent (DirectOrder a) (Set a)]
  -- ^ The result is potentially several connetcted components have no
  -- tasks from given one
cleanSequentialGraph diror cc =
  let
    restDirors = graphNodes $ unConnectedComponent cc
    resGraph = flip evalState S.empty $ do
      void $ cleanDirectOrder diror -- NOTE: just to prepare state
      AnyOrder res <- cleanAnyOrder $ AnyOrder restDirors
      return $ connectedComponents directOrderCommonElems $ S.toList res
  in resGraph

deintersectPlan :: (Ord a) => ProjectPlan a -> Variants (ProjectPlan a)
deintersectPlan = \case
  DirectOrderPlan direct -> DirectOrderPlan <$> deintersectSequence direct
  AnyOrderPlan ao        -> AnyOrderPlan    <$> deintersectProduct ao

deintersectSequence :: (Ord a) => DirectOrder a -> Variants (DirectOrder a)
deintersectSequence = \case
  DirectOrder aos    -> DirectOrder <$> traverse deintersectProduct aos
  t@(PlannedTask {}) -> return t

deintersectProduct
  :: (Ord a)
  => AnyOrder a
  -> Variants (AnyOrder a)
deintersectProduct (AnyOrder (S.toList -> directs)) = do
  let ccs = connectedComponents directOrderCommonElems directs
  plans <- traverse planConnectedComponent ccs
  return $ anyOrder plans

planConnectedComponent
  :: forall a. (Ord a)
  => ConnectedComponent (DirectOrder a) (Set a)
  -> Variants (ProjectPlan a)
planConnectedComponent cc = do
  let
    conMap :: Map Int [DirectOrder a]
    conMap = M.fromListWith (++)
      $ fmap (snd &&& ((:[]) . fst))
      $ M.toList
      $ edgesCount cc
    mostConnected = case M.toDescList conMap of
      []         -> mempty
      ((_, a):_) -> a
  cuttedNode <- variants mostConnected
  headPlan <- deintersectSequence cuttedNode
  let least = cleanSequentialGraph cuttedNode cc
  tailPlans <- traverse planConnectedComponent least
  return $ DirectOrderPlan $ directOrder
    [ DirectOrderPlan headPlan
    , AnyOrderPlan $ anyOrder tailPlans ]
