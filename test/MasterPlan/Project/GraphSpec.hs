module MasterPlan.Project.GraphSpec where

import Control.Applicative as A
import Control.Monad
import Data.Foldable as F
import Data.List as L
import Data.Monoid
import Data.Set as S
import MasterPlan.Project.Graph
import Test.HUnit
import Test.Hspec
import Test.Hspec.SmallCheck
import Test.SmallCheck
import Test.SmallCheck.Series

ccFromTuples :: [(Int, Int)] -> [ConnectedComponent Int ()]
ccFromTuples as =
  let
    h = S.fromList as
    els = S.toList $ S.fromList
      $ fmap fst as ++ fmap snd as
    haveEdge a b = if S.member (a, b) h || S.member (b, a) h then Just () else Nothing
    ccs = connectedComponents haveEdge els
  in ccs

-- -- | Remove hanging nodes
-- cleanGraph :: Graph Int () -> Graph Int ()
-- cleanGraph g =
--   let
--     toNodes = S.unions $ snd <$> M.toList g
--     res = M.filterWithKey nonEmpty g
--     nonEmpty k v = not $ S.member k toNodes && S.null v
--   in res

propDoubleSplit :: [GraphTuple] -> Either Reason Reason
propDoubleSplit tuples = do
  let
    ccs :: [ConnectedComponent Int ()]
    ccs = ccFromTuples $ fmap unGraphTuple tuples
    subCcs :: [[ConnectedComponent Int ()]]
    subCcs = (graphComponents . unConnectedComponent) <$> ccs
  for_ subCcs $ \cc -> case L.length cc of
    1 -> return ()
    _ -> Left $ show cc
  let
    merged = F.foldl' (graphUnion $ const id) emptyGraph
      $ unConnectedComponent <$> concat subCcs
    originalMerged = F.foldl' (graphUnion $ const id) emptyGraph
      $ unConnectedComponent <$> ccs
  unless (merged == originalMerged)
    $ Left $ "merged: " <> show merged <> " but original was: " <> show originalMerged
  let
    splittedAgain = S.fromList $ graphComponents merged
    originalSplitted = S.fromList ccs
  unless (splittedAgain == originalSplitted)
    $ Left $ "splitted again: " <> show splittedAgain <> " but orignal: " <> show originalSplitted
  return "Ok"

newtype GraphTuple = GraphTuple
  { unGraphTuple :: (Int, Int)
  } deriving (Eq, Show)

instance (Monad m) => Serial m GraphTuple where
  series = generate $ \d ->
    if d >= 0 then go d else A.empty
    where
      go d = do
        let x = min d 5
        (a:as) <- L.tails $ [0..x]
        b <- as
        return $ GraphTuple (a, b)

spec :: Spec
spec = describe "Graph spec" $ do
  describe "Cases" $ do
    it "Simple case 1" $ do
      let comps = ccFromTuples [(1, 2), (3, 2)]
      L.length comps @?= 1
    it "Cycle is one cc" $ do
      let comps = ccFromTuples [(1, 2), (2, 3), (3, 4), (4, 1)]
      L.length comps @?= 1
    it "2 Cycles 2 cc" $ do
      let comps = ccFromTuples
            [ (1, 2), (2, 3), (3, 1)
            , (10, 11), (11, 12), (12, 10)]
      L.length comps @?= 2
    it "Line breaks to 2 cc" $ do
      let
        comps = ccFromTuples
          [ (1, 2), (2, 3), (3, 4), (4, 5) ]
        res = S.fromList
          $ fmap unConnectedComponent
          $ comps >>= removeNode 3
        expected = S.fromList
          $ fmap unConnectedComponent
          $ ccFromTuples [ (1, 2), (4, 5) ]
      res @?= expected


  describe "Props" $ do
    it "No double split" $ property
      $ changeDepth (const 6)
      $ forAll propDoubleSplit
