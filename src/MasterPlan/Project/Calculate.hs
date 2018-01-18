module MasterPlan.Project.Calculate where

import Control.Arrow
import Control.Lens
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

-- -- | List of elements where each subsequent element depends from
-- -- previous.
-- newtype SequencePlan a = SequencePlan
--   { unSequencePlan :: [ProjectPlan a]
--   } deriving (Eq)

-- sequenceLen :: SequencePlan a -> Int
-- sequenceLen (SequencePlan a) = L.length a

-- sequencePlan :: SequencePlan Task -> ProjectPlan Task
-- sequencePlan (SequencePlan a) = DirectOrder $ cleanPlansSequence a

-- -- | Remove tasks from subsequent plans which are already executed
-- cleanPlansSequence :: [ProjectPlan Task] -> [ProjectPlan Task]
-- cleanPlansSequence = error "Not implemented: cleanPlansSequence"


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
planProject = \case
  PTask t      -> variants [point t]
  PComposite c -> planAlgebra $ c ^. algebra

planAlgebra
  :: (Ord a)
  => Algebra (Project a)
  -> Variants (ProjectPlan Task)
planAlgebra = \case
  Sum ones      -> variants ones >>= planAlgebra
  Product algs  -> planProduct algs
  Sequence algs -> planSequence algs
  Atom p        -> planProject p

planSequence
  :: (Ord a)
  => [Algebra (Project a)]
  -> Variants (ProjectPlan Task)
planSequence = error "Not implemented: planSequence"

planProduct
  :: forall a. (Ord a)
  => [Algebra (Project a)]
  -> Variants (ProjectPlan Task)
planProduct algs = do
  let
    connected :: [ConnectedComponent (Algebra (Project a))]
    connected = connectedComponents algebraConnected algs
  case connected of
    [single] -> connectedAlgebrasPlan single
    _        -> do
      independents <- traverse connectedAlgebrasPlan connected
      return $ AnyOrderPlan $ anyOrder independents

algebraConnected
  :: (Ord a)
  => Algebra (Project a)
  -> Algebra (Project a)
  -> Bool
algebraConnected a b = not $ S.null
  $ S.intersection (S.fromList $ allSubprojects a) (S.fromList $ allSubprojects b)

allSubprojects :: Algebra (Project a) -> [Project a]
allSubprojects a = do
  proj <- F.toList a
  let nestedProjs = (proj ^.. _PComposite . algebra) >>= allSubprojects
  proj : nestedProjs

-- | Plan algebras having common projects. All algebras are depend
-- with each other. Meaning they form non-strongly connected graph
-- where each vertex is an algebra and each edge is the fact that two
-- algebras have common projects
connectedAlgebrasPlan
  :: (Ord a)
  => ConnectedComponent (Algebra (Project a))
  -> Variants (ProjectPlan Task)
connectedAlgebrasPlan algs =
  let
    -- Plans grouped by length. Current heuristic is to take shortest
    -- plans from whole list to minimize computational overhead
    plansLenMap :: Map Int (Variants (DirectOrder Task))
    plansLenMap
      = M.fromListWith (<>)
      $ unVariants
      $ fmap (directOrderLength &&& pure)
      $ possibleDirectPlans algs
    directPlans = case M.toAscList plansLenMap of
      []             -> mempty
      ((_, plans):_) -> plans
  in DirectOrderPlan <$> directPlans

possibleDirectPlans
  :: forall a. (Ord a)
  => ConnectedComponent (Algebra (Project a))
  -- ^ Algebras having common projects in formula
  -> Variants (DirectOrder Task)
  -- ^ Variants of sequences. Each list in variant is a payload for
  -- 'DirectOrder' constructor
possibleDirectPlans algs = do
  let
    conMap :: Map Int [Algebra (Project a)]
    conMap = M.fromListWith (++)
      $ fmap (snd &&& ((:[]) . fst))
      $ M.toList
      $ edgesCount algs
    mostConnected = case M.toDescList conMap of
      []         -> mempty
      ((_, a):_) -> a
  cuttedNode <- variants mostConnected
  headPlan <- planAlgebra cuttedNode
  let independents = removeNode cuttedNode algs
  tailPlans <- traverse possibleDirectPlans independents
  return $ directOrder
    [ headPlan
    , AnyOrderPlan $ AnyOrder $ S.fromList tailPlans ]
