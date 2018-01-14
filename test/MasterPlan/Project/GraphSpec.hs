module MasterPlan.Project.GraphSpec where

import Control.Applicative as A
import Data.Foldable
import Data.List as L
import Data.Set as S
import MasterPlan.Project.Graph
import Test.Hspec
import Test.Hspec.SmallCheck
import Test.SmallCheck
import Test.SmallCheck.Series


propDoubleSplit :: [(Int, Int)] -> Either Reason Reason
propDoubleSplit as = do
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
  return "Ok"

depTuple :: (Monad m) => Series m (Int, Int)
depTuple = generate $ \d ->
  if d >= 0 then go d else A.empty
  where
    go d = L.take 10 $ do
      (a:as) <- L.tails $ L.reverse [0..d]
      b <- as
      return (a, b)

genInts :: (Monad m) => Series m [(Int, Int)]
genInts = cons0 [] \/ intsCons
  where
    intsCons = decDepth $ (:) <$> depTuple <~> genInts

spec :: Spec
spec = describe "Graph spec" $ do
  it "No double split" $ property
    $ changeDepth (const 6)
    $ over genInts propDoubleSplit
