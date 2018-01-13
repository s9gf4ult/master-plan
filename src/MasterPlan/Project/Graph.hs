module MasterPlan.Project.Graph where

import Control.Lens
import Data.Graph
import Data.Map.Strict as M
import Data.Vector as V

-- | Set of dependent tasks, projects or algebras. Meaning that all
-- elements in a list form connected graph where each vertex is
-- element and each edge is some kind of dependency between nodes
data ConnectedComponent a = ConnectedComponent
  { _connectedComponentGraph :: Graph
  , _connectedComponentElems :: Vector a
  }

makeFields ''ConnectedComponent

removeConnectedComponent
  :: (Eq a)
  => a
  -- ^ Element to remove from dependent collection
  -> ConnectedComponent a
  -> [ConnectedComponent a]
  -- ^ Result is list of independent from each other dependent
  -- collections elements
removeConnectedComponent a cc = case V.elemIndex a v of
  Nothing -> pure cc
  Just idx ->

  where
    ConnectedComponent g v = cc



mkConnectedComponents
  :: (a -> a -> Bool)
  -- ^ Check if two elems are dependent
  -> Vector a
  -> [ConnectedComponent a]
mkConnectedComponents f v =
  let
    comps = components $ graphFromVector f v
    res   = ccFromForest v <$> comps
  in res

graphFromVector :: (a -> a -> Bool) -> Vector a -> Graph
graphFromVector = error "Not implemented: graphFromVector"

ccFromForest :: Vector a -> Tree Vertex -> ConnectedComponent a
ccFromForest = error "Not implemented: ccFromForest"


-- | Ther key of result is the count of connections with other elements.
connectivityMap :: ConnectedComponent a -> Map Int [a]
connectivityMap = error "Not implemented: connectivityMap"
