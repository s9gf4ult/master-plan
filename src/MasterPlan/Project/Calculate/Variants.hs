module MasterPlan.Project.Calculate.Variants where


import Control.Monad.State.Strict
import Data.Set as S
import MasterPlan.Algebra
import MasterPlan.Internal.Import
import MasterPlan.Project.Graph
import MasterPlan.Project.Plan
import MasterPlan.Project.Struct


-- | Newtype over list to distinguish just list of plans from list of
-- independent plans, or in other words list of plan variants.
newtype Variants a = Variants
  { unVariants :: [a]
  } deriving (Eq, Monoid, Functor, Foldable, Applicative, Monad)

variants :: [a] -> Variants a
variants = Variants

bestVariant :: Variants (Project Calculation) -> Project Calculation
bestVariant = error "Not implemented: bestVariant"
