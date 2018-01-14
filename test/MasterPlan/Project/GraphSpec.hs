module MasterPlan.Project.GraphSpec where

import Data.List as L
import Data.Set as S
import MasterPlan.Project.Graph
import Test.HUnit
import Test.Hspec
import Test.Hspec.SmallCheck
import Test.SmallCheck
import Test.SmallCheck.Series


propDoubleSplit :: [(Int, Int)] -> Bool
propDoubleSplit as =
  let
    h = S.fromList as
    els = S.toList $ S.fromList
      $ fmap fst as ++ fmap snd as
    haveEdge a b = S.member (a, b) h || S.member (b, a) h
    ccs = connectedComponents haveEdge els
    subCcs :: [[Graph Int]]
    subCcs = (graphComponents . unConnectedComponent) <$> ccs
    ok = L.all (\a -> L.length a == 1) subCcs
  in ok



spec :: Spec
spec = describe "Graph spec" $ do
  it "No double split" $ property $ forAll propDoubleSplit
