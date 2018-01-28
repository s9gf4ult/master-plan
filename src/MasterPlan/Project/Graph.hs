module MasterPlan.Project.Graph
  ( ConnectedComponent(unConnectedComponent)
  , graphComponents
  , removeNode
  , edgesCount
  , Graph(unGraph)
  , emptyGraph
  , newGraph
  , graphNodes
  , graphUnion
  , graphInsert
  , connectedComponents
  ) where

import Data.Foldable as F
import Data.List as L
import Data.Map.Strict as M
import Data.Set as S
import MasterPlan.Internal.Import

-- | Undirected graph optimized for changing.
newtype Graph node edge = Graph
  { unGraph :: Map node (Map node edge)
  } deriving (Eq, Ord, Show)

graphNull :: Graph a e -> Bool
graphNull (Graph m) = M.null m

newGraph :: (Ord a) => (a -> a -> Maybe e) -> [a] -> Graph a e
newGraph f els = Graph $ M.fromListWith M.union $ do
  (a:bs) <- L.tails els
  b <- bs
  e <- F.toList $ f a b
  return (a, M.singleton b e)

graphNodes :: (Ord a) => Graph a e -> Set a
graphNodes (Graph g) = S.union (M.keysSet g)
  $ S.unions $ M.keysSet <$> M.elems g

graphInsert
  :: (Ord a)
  => (e -> e -> e)
  -- ^ Merge edges if got same edges
  -> a
  -- ^ Node from
  -> Map a e
  -- ^ Node and edges to
  -> Graph a e
  -> Graph a e
graphInsert f a bs (Graph g) = Graph
  $ M.insertWith (M.unionWith f) a bs g

graphUnion :: (Ord a) => (e -> e -> e) -> Graph a e -> Graph a e -> Graph a e
graphUnion f (Graph g1) (Graph g2) = Graph
  $ M.unionWith (M.unionWith f) g1 g2

emptyGraph :: Graph a e
emptyGraph = Graph M.empty

graphRemoveNode :: (Ord a) => a -> Graph a e -> Graph a e
graphRemoveNode a (Graph g) = Graph $ M.fromListWith M.union $ do
  (k, v) <- M.toList g
  guard $ k /= a
  let r = M.delete a v
  guard $ not $ M.null r
  return $ (k, r)

-- | Set of dependent tasks, projects or algebras. Meaning that all
-- elements in a list form undirected connected graph where each
-- vertex is element and each edge is some kind of dependency between
-- nodes
newtype ConnectedComponent node edge = ConnectedComponent
  { unConnectedComponent :: (Graph node edge)
  } deriving (Eq, Ord, Show)

removeNode
  :: (Ord a)
  => a
  -- ^ Element to remove from dependent collection
  -> ConnectedComponent a e
  -> [ConnectedComponent a e]
  -- ^ Result is list of independent from each other dependent
  -- collections elements
removeNode a (ConnectedComponent g) =
  let removed = graphRemoveNode a g
  in graphComponents removed

graphComponents :: forall a e. (Ord a) => Graph a e -> [ConnectedComponent a e]
graphComponents g = ConnectedComponent <$> go [] emptyCurrent g
  where
    emptyCurrent = (S.empty, emptyGraph)

    insertKV k v (curS, curG) =
      ( S.insert k $ S.union (M.keysSet v) curS
      , graphInsert (const id) k v curG )

    go :: [(Set a, Graph a e)] -> (Set a, Graph a e) -> Graph a e -> [Graph a e]
    go acc current graph = case M.minViewWithKey $ unGraph graph of
      Nothing                    -> fmap snd $ nonEmptyCurrent acc current
      Just ((k, v), Graph -> cuttedGraph) ->
        let (newCurrent, newGraph) = wideCut (M.keys v) (insertKV k v current) cuttedGraph
        in go (appendAcc acc newCurrent) emptyCurrent newGraph

    wideCut :: [a] -> (Set a, Graph a e) -> Graph a e -> ((Set a, Graph a e), Graph a e)
    wideCut [] current graph = (current, graph)
    wideCut (a:as) current graph =
      let (more, Graph -> cuttedGraph)
            = M.updateLookupWithKey (const $ const Nothing) a $ unGraph graph
      in case more of
          Nothing -> wideCut as                current                 cuttedGraph
          Just vs -> wideCut (as ++ M.keys vs) (insertKV a vs current) cuttedGraph

    appendAcc :: [(Set a, Graph a e)] -> (Set a, Graph a e) -> [(Set a, Graph a e)]
    appendAcc [] a = [a]
    appendAcc ((acc@(accS, accG)):rest) cur@(curS, curG) =
      if S.null $ S.intersection accS curS
      then cur:acc:rest
      else (S.union accS curS, graphUnion (const id) accG curG):rest

    nonEmptyCurrent [] x = [x]
    nonEmptyCurrent as current@(_, g) = if graphNull g
      then as   -- To prevent adding empty graph as last element every time
      else current : as

connectedComponents
  :: (Ord a)
  => (a -> a -> Maybe e)
  -- ^ Check if two elems are dependent
  -> [a]
  -> [ConnectedComponent a e]
connectedComponents f v =
  graphComponents $ newGraph f v

edgesCount :: (Ord a) => ConnectedComponent a e -> Map a Int
edgesCount (ConnectedComponent (Graph g)) = M.fromListWith (+) $ do
  (f, t) <- M.toList g
  (f, M.size t):(L.zip (M.keys t) $ repeat 1)
