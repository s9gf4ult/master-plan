module MasterPlan.Input.Expression where

import Control.Lens
import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Char
import Data.HashMap.Strict
import Data.Hashable
import Data.List as L
import Data.List.NonEmpty as NE
import Data.Scientific
import Data.Text as T
import Data.Void
import GHC.Generics (Generic)
import MasterPlan.Algebra
import MasterPlan.Internal.TH
import Text.Megaparsec
import Text.Megaparsec.Expr

import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer

type Megaparsec = Parsec Void T.Text


newtype ProjectName = ProjectName
  { unProjectName :: NonEmpty Text
  } deriving (Eq, Hashable)

instance FromJSON ProjectName where
  parseJSON = withText "Project name" parseProjectName

instance FromJSONKey ProjectName where
  fromJSONKey = FromJSONKeyTextParser parseProjectName

projectName :: Megaparsec ProjectName
projectName = ProjectName <$> dotedAlphaNum

parseProjectName :: Text -> Parser ProjectName
parseProjectName = eitherJsonParser . runParser projectName ""

newtype ModuleName = ModuleName
  { unModuleName :: NonEmpty Text
  } deriving (Eq)

instance FromJSON ModuleName where
  parseJSON = withText "Dot separated module name" parseModuleName

moduleName :: Megaparsec ModuleName
moduleName = ModuleName <$> dotedAlphaNum

parseModuleName :: Text -> Parser ModuleName
parseModuleName = eitherJsonParser . runParser moduleName ""

data Expression = Expression (Algebra ProjectName)
  deriving (Eq)

instance FromJSON Expression where
  parseJSON = withText "Project expression" go
    where
      go = (error "FIXME: parse expression")

data ModuleImport = ModuleImport
  { _miModule  :: ModuleName
  , _miSynonym :: Maybe ModuleName
  } deriving (Eq)

instance FromJSON ModuleImport where
  parseJSON = withText "Module import" go
    where
      go t = case T.splitOn " as " t of
        [m] -> do
          moduleName <- parseModuleName m
          return $ ModuleImport moduleName Nothing
        [m, syn] -> do
          moduleName <- parseModuleName m
          synonym <- parseModuleName syn
          return $ ModuleImport moduleName $ Just synonym
        _ -> fail "Unexpected count of \"as\" keywords in import"

-- | Parses the part of right-hand-side after the optional properties
--  (literal string title or properties between curly brackets)
expression ∷ Megaparsec a -> Megaparsec (Algebra a)
expression subexpr = makeExprParser term table <?> "expression"
  where
    term =  parens (expression subexpr) <|> (Atom <$> subexpr)
    table = [[binary "*" (combineWith Product)]
            ,[binary "->" (combineWith Sequence)]
            ,[binary "+" (combineWith Sum)]]
    binary  op f = InfixL (f <$ symbol op)
    combineWith c p1 p2 = c [p1, p2]

dotedAlphaNum :: Megaparsec (NonEmpty Text)
dotedAlphaNum = do
  a <- sepBy1 (T.pack <$> some Char.alphaNumChar) (Char.char '.')
  case NE.nonEmpty a of
    Nothing -> fail "Name can not be empty"
    Just a  -> return a

eitherJsonParser :: (Show e) => Either e a -> Parser a
eitherJsonParser = \case
  Left e  -> fail $ show e
  Right a -> return a

symbol ∷ T.Text → Megaparsec T.Text
symbol = Lexer.symbol Char.space

-- | 'parens' parses something between parenthesis.
parens ∷ Megaparsec a → Megaparsec a
parens = between (symbol "(") (symbol ")")
