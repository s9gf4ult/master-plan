module MasterPlan.InputSpec (spec) where

import Control.Lens
import Data.Foldable
import Data.HashMap.Strict as H
import Data.Text as T
import MasterPlan.Algebra
import MasterPlan.Input.Yaml
import Test.HUnit
import Test.Hspec
import Text.Megaparsec

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

shortenProject :: Module
shortenProject = Module
  { _mModule = "shorten"
  , _mImports = Nothing
  , _mRoot = Nothing
  , _mProjects = H.fromList
    [("a", a)]
  }
  where
    a = emptyProject
      & pTitle .~ Just "A project"
      & pDescription .~ Just "A description"
      & pExpression .~ Just e
    e = Expression $ Sum [Atom "x", Atom "y"]

onlyExprProject :: Module
onlyExprProject = Module
  { _mModule = "onlyExpr"
  , _mImports = Nothing
  , _mRoot = Nothing
  , _mProjects = H.fromList
    [("a", a)]
  }
  where
    a = emptyProject
      & pExpression .~ Just e
    e = Expression $ Sum [Atom "x", Atom "y"]

dotedProject :: Module
dotedProject = Module
  { _mModule = "dotedProjects"
  , _mImports = Just [ModuleImport "simple" $ Just "s"]
  , _mRoot = Nothing
  , _mProjects = H.fromList
    [("a", a)]
  }
  where
    a = emptyProject
      & pExpression .~ Just e
    e = Expression $ Sequence [Atom "s.a", Atom "s.b"]

lensyEq
  :: (Eq a, Show a)
  => Module
  -> Module
  -> Fold Module a
  -> Assertion
lensyEq a b l = do
  let x = a ^?! l
      y = b ^?! l
  x @?= y

parserSpec
  :: Algebra ProjectName
  -- ^ Expected result
  -> [Text]
  -- ^ Variants that must be parsed to result
  -> Assertion
parserSpec expected ts = for_ ts $ \t -> do
  let res = over _Left parseErrorPretty $ runParser (expression projectName) "" t
  assertEqual (T.unpack t) (Right expected) res

spec :: Spec
spec = do
  describe "Yaml parser" $ do
    it "simple project" $ do
      Right m <- parseYamlModule "test/fixtures/simple.yaml"
      lensyEq m simpleProject $ mModule
      lensyEq m simpleProject $ mProjects . at "a"
      lensyEq m simpleProject $ mProjects . at "b"
      lensyEq m simpleProject $ mProjects . at "root"
      m @?= simpleProject
    it "shorten keywords" $ do
      m <- parseYamlModule "test/fixtures/shorten.yaml"
      m @?= Right shortenProject
    it "only expression" $ do
      m <- parseYamlModule "test/fixtures/onlyExpr.yaml"
      m @?= Right onlyExprProject
    it "doted names in expression" $ do
      m <- parseYamlModule "test/fixtures/dotedProjects.yaml"
      m @?= Right dotedProject
  describe "Expressions parser" $ do
    it "atom parser" $ do
      parserSpec (Atom "a")
        ["a", " a", "a ", "(a)", "(  (a  ))"]
    it "sum parser" $ do
      parserSpec (Sum [Atom "a", Atom "b"])
        ["a+b", "a+ b", " a + b ", "(a+ (b ))", "(a) + (b)"]
    it "prod parser" $ do
      parserSpec (Product [Atom "a", Atom "b"])
        ["a * b", "(a)*b", "a*b"]
    it "sum of products parser" $ do
      parserSpec (Sum [ Product [Atom "a", Atom "b"]
                      , Product [Atom "c", Atom "d"]])
        ["a * b + c * d", "(a * b) + c * d", "a*b+c*d"]
