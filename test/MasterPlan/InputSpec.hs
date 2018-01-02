module MasterPlan.InputSpec (spec) where

import Control.Lens
import Data.HashMap.Strict as H
import MasterPlan.Algebra
import MasterPlan.Input
import Test.HUnit
import Test.Hspec

simpleProject :: Module
simpleProject = Module
  { _mModule = "simple"
  , _mImports = Nothing
  , _mRoot = Just "root"
  , _mProjects = H.fromList
    [ ("root", root)
    , ("a", a)
    , ("b", b)
    ]
  }
  where
    root = emptyProject
      & pExpression .~ Just e
    e = Expression $ Sum [Atom "a", Atom "b"]
    a = emptyProject
      & pTitle .~ Just "A project"
    b = emptyProject
      & pTitle .~ Just "B project"

spec :: Spec
spec = do
  it "simple project" $ do
    m <- parseYamlModule "test/fixtures/simple.yaml"
    m @?= Right simpleProject
