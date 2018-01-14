module MasterPlan.Project.Graph
  ( ConnectedComponent(unConnectedComponent)
  , Graph
  , graphComponents
  , removeNode
  , connectedComponents
  , edgesCount
  )
where

import Data.List as L
import Data.Map.Strict as M
import Data.Set as S
import MasterPlan.Internal.Import

-- | Set of dependent tasks, projects or algebras. Meaning that all
-- elements in a list form undirected connected graph where each
-- vertex is element and each edge is some kind of dependency between
-- nodes
newtype ConnectedComponent a = ConnectedComponent
  { unConnectedComponent :: (Graph a)
  }

type Graph a = Map a (Set a)

removeNode
  :: (Ord a)
  => a
  -- ^ Element to remove from dependent collection
  -> ConnectedComponent a
  -> [ConnectedComponent a]
  -- ^ Result is list of independent from each other dependent
  -- collections elements
removeNode a (ConnectedComponent cc) =
  let removed = S.delete a <$> M.delete a cc
  in ConnectedComponent <$> graphComponents removed

graphComponents :: forall a. (Ord a) => Graph a -> [Graph a]
graphComponents = go [] emptyCurrent
  where
    emptyCurrent = (S.empty, M.empty)

    insertKV k v (curS, curG) = (S.insert k $ S.union v curS, M.insertWith S.union k v curG)

    go :: [(Set a, Graph a)] -> (Set a, Graph a) -> Graph a -> [Graph a]
    go acc current graph = case M.minViewWithKey graph of
      Nothing                    -> fmap snd $ current : acc
      Just ((k, v), cuttedGraph) ->
        let (newCurrent, newGraph) = wideCut (S.toList v) (insertKV k v current) cuttedGraph
        in go (appendAcc acc newCurrent) emptyCurrent newGraph

    wideCut :: [a] -> (Set a, Graph a) -> Graph a -> ((Set a, Graph a), Graph a)
    wideCut [] current graph = (current, graph)
    wideCut (a:as) current graph =
      let (more, cuttedGraph) = M.updateLookupWithKey (const $ const Nothing) a graph
      in case more of
          Nothing -> wideCut as                  current                 cuttedGraph
          Just vs -> wideCut (as ++ S.toList vs) (insertKV a vs current) cuttedGraph

    appendAcc :: [(Set a, Graph a)] -> (Set a, Graph a) -> [(Set a, Graph a)]
    appendAcc [] a = [a]
    appendAcc ((acc@(accS, accG)):rest) cur@(curS, curG) = if S.null $ S.intersection accS curS
      then cur:acc:rest
      else (S.union accS curS, M.unionWith S.union accG curG):rest

connectedComponents
  :: (Ord a)
  => (a -> a -> Bool)
  -- ^ Check if two elems are dependent
  -> [a]
  -> [ConnectedComponent a]
connectedComponents f v =
  fmap ConnectedComponent $ graphComponents $ mkGraph f v

mkGraph :: (Ord a) => (a -> a -> Bool) -> [a] -> Graph a
mkGraph f els = M.fromListWith S.union $ do
  (a:bs) <- L.tails els
  b <- bs
  guard $ f a b
  return (a, S.singleton b)

edgesCount :: (Ord a) => ConnectedComponent a -> Map a Int
edgesCount (ConnectedComponent g) = M.fromListWith (+) $ do
  (f, t) <- M.toList g
  (f, S.size t):(L.zip (S.toList t) $ repeat 1)
