module MasterPlan.AlgebraSpec where

import Data.List as L
import MasterPlan.Algebra
import Test.HUnit
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Arbitrary.Generic
import Test.QuickCheck.Gen

instance (Arbitrary a) => Arbitrary (Algebra a) where
  arbitrary = resize 3 $ genericArbitrary
  shrink    = genericShrink

propFlatten :: Algebra Int -> Bool
propFlatten a = go $ flatten a
  where
    go = \case
      Atom _      -> True
      Sum as      -> L.all noSums as && L.all go as
      Product as  -> L.all noProds as && L.all go as
      Sequence as -> L.all noSeqs as && L.all go as
      where
        noSums, noProds, noSeqs :: Algebra Int -> Bool
        noSums = \case
          Sum _ -> False
          _     -> True
        noProds = \case
          Product _ -> False
          _         -> True
        noSeqs = \case
          Sequence _ -> False
          _          -> True

spec :: Spec
spec = do
  describe "Algebra flattener" $ do
    it "flattens sums" $ do
      let
        a :: Algebra Int
        a  = Sum [Atom 1, Sum [Atom 2, Sum [Atom 3, Atom 4]]]
        ex = Sum [Atom 1, Atom 2, Atom 3, Atom 4] :: Algebra Int
      flatten a @?= ex
    it "flattens sums of products" $ do
      let
        a :: Algebra Int
        a = Sum [Atom 1, Product [Atom 2, Atom 3], Sum [Product [Atom 4, Atom 5], Atom 6]]
        ex = Sum [Atom 1, Product [Atom 2, Atom 3], Product [Atom 4, Atom 5], Atom 6]
      flatten a @?= ex
    it "flattens sums of products of sums" $ do
      let
        a :: Algebra Int
        a = Sum [ Product [ Product [ Atom 1
                                    , Sum [Atom 2, Atom 3]]
                          , Atom 4
                          , Product [Atom 5]]
                , Sum [Atom 6] ]
        ex = Sum [ Product [ Atom 1
                           , Sum [Atom 2, Atom 3]
                           , Atom 4
                           , Atom 5 ]
                 , Atom 6
                 ]
      flatten a @?= ex
    prop "flattens all subtrees" propFlatten
