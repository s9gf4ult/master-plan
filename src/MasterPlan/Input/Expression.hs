module MasterPlan.Input.Expression where

import Data.Aeson
import Data.Aeson.Types
import Data.Hashable
import Data.List.NonEmpty as NE
import Data.String
import Data.Text as T
import Data.Void
import MasterPlan.Algebra
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr

import qualified Text.Megaparsec.Char.Lexer as Lex

type Megaparsec = Parsec Void T.Text

newtype ProjectName = ProjectName
  { unProjectName :: NonEmpty Text
  } deriving (Eq, Show, Hashable)

instance FromJSON ProjectName where
  parseJSON = withText "Project name" parseProjectName

instance FromJSONKey ProjectName where
  fromJSONKey = FromJSONKeyTextParser parseProjectName

instance IsString ProjectName where
  fromString s = case runParser projectName "" $ T.pack s of
    Left e  -> error $ show e
    Right a -> a

projectName :: Megaparsec ProjectName
projectName = ProjectName <$> dotedAlphaNum

parseProjectName :: Text -> Parser ProjectName
parseProjectName = eitherJsonParser . runParser projectName ""

newtype ModuleName = ModuleName
  { unModuleName :: NonEmpty Text
  } deriving (Eq, Show)

instance FromJSON ModuleName where
  parseJSON = withText "Dot separated module name" parseModuleName

-- | May throw exceptions. Use only in tests and similar stuff
instance IsString ModuleName where
  fromString s = case runParser moduleName "" $ T.pack s of
    Left e  -> error $ show e
    Right a -> a

moduleName :: Megaparsec ModuleName
moduleName = ModuleName <$> dotedAlphaNum

parseModuleName :: Text -> Parser ModuleName
parseModuleName = eitherJsonParser . runParser moduleName ""

data Expression = Expression (Algebra ProjectName)
  deriving (Eq, Show)

instance FromJSON Expression where
  parseJSON = withText "Project expression" parseExpression

parseExpression :: Text -> Parser Expression
parseExpression t
  = fmap Expression $ eitherJsonParser
  $ runParser (expression projectName) "" t

data ModuleImport = ModuleImport
  { _miModule  :: ModuleName
  , _miSynonym :: Maybe ModuleName
  } deriving (Eq, Show)

instance FromJSON ModuleImport where
  parseJSON = withText "Module import" go
    where
      go t = case T.splitOn " as " t of
        [m] -> do
          mn <- parseModuleName m
          return $ ModuleImport mn Nothing
        [m, syn] -> do
          mn <- parseModuleName m
          synonym <- parseModuleName syn
          return $ ModuleImport mn $ Just synonym
        _ -> fail "Unexpected count of \"as\" keywords in import"

dotedAlphaNum :: Megaparsec (NonEmpty Text)
dotedAlphaNum = do
  a <- sepBy1 (T.pack <$> some alphaNumChar) (char '.')
  case NE.nonEmpty a of
    Nothing -> fail "Name can not be empty"
    Just t  -> return t

eitherJsonParser
  :: (ShowErrorComponent e, Ord t, ShowToken t)
  => Either (ParseError t e) a
  -> Parser a
eitherJsonParser = \case
  Left e  -> fail $ parseErrorPretty e
  Right a -> return a

-- | Parses the part of right-hand-side after the optional properties
--  (literal string title or properties between curly brackets)
expression
  :: Megaparsec a
  -> Megaparsec (Algebra a)
expression subexpr = space *> go
  where
    go    = makeExprParser term table <?> "expression"
    term  = parens go <|> (Atom <$> lexeme subexpr)
    table = [[binary "*" (combineWith Product)]
            ,[binary "->" (combineWith Sequence)]
            ,[binary "+" (combineWith Sum)]]
    binary  op f = InfixL (f <$ symbol op)
    combineWith c p1 p2 = c [p1, p2]

-- | Consume leading spaces (if exists) then given parser, then
-- trailing spaces
spaced :: Megaparsec a -> Megaparsec a
spaced p = do
  try space -- may consume nothing
  lexeme p

lexeme :: Megaparsec a -> Megaparsec a
lexeme = Lex.lexeme space

symbol ∷ T.Text → Megaparsec T.Text
symbol = Lex.symbol space

-- | 'parens' parses something between parenthesis.
parens ∷ Megaparsec a → Megaparsec a
parens = between (symbol "(") (symbol ")")
