module MasterPlan.Project.GraphSpec where

import Control.Applicative as A
import Control.Monad
import Data.Foldable
import Data.List as L
import Data.Map.Strict as M
import Data.Monoid
import Data.Set as S
import MasterPlan.Project.Graph
import Test.Hspec
import Test.Hspec.SmallCheck
import Test.SmallCheck
import Test.SmallCheck.Series


propDoubleSplit :: [GraphTuple] -> Either Reason Reason
propDoubleSplit (fmap unGraphTuple -> as) = do
  let
    h = S.fromList as
    els = S.toList $ S.fromList
      $ fmap fst as ++ fmap snd as
    haveEdge a b = S.member (a, b) h || S.member (b, a) h
    ccs = connectedComponents haveEdge els
    subCcs :: [[Graph Int]]
    subCcs = (graphComponents . unConnectedComponent) <$> ccs
  for_ subCcs $ \cc -> case L.length cc of
    1 -> return ()
    _ -> Left $ show cc
  let
    merged = M.unionsWith S.union $ concat subCcs
    originalMerged = M.unionsWith S.union $ unConnectedComponent <$> ccs
  unless (merged == originalMerged)
    $ Left $ "merged: " <> show merged <> " but original was: " <> show originalMerged
  let
    splittedAgain = S.fromList $ graphComponents merged
    originalSplitted = S.fromList $ unConnectedComponent <$> ccs
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
  describe "Props" $ do
    it "No double split" $ property
      $ changeDepth (const 6)
      $ forAll propDoubleSplit
