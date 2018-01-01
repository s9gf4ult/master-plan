module MasterPlan.Input.Expression where

import Data.Text as T
import Data.Void
import MasterPlan.Algebra
import Text.Megaparsec
import Text.Megaparsec.Expr

import qualified Text.Megaparsec.Char as Char
import qualified Text.Megaparsec.Char.Lexer as Lexer


type Parser = Parsec Void T.Text


symbol ∷ T.Text → Parser T.Text
symbol = Lexer.symbol Char.space

-- | 'parens' parses something between parenthesis.
parens ∷ Parser a → Parser a
parens = between (symbol "(") (symbol ")")

-- |Parses the part of right-hand-side after the optional properties
--  (literal string title or properties between curly brackets)
expression ∷ Parser a -> Parser (Algebra a)
expression subexpr = makeExprParser term table <?> "expression"
  where
    term =  parens (expression subexpr) <|> (Atom <$> subexpr)
    table = [[binary "*" (combineWith Product)]
            ,[binary "->" (combineWith Sequence)]
            ,[binary "+" (combineWith Sum)]]
    binary  op f = InfixL (f <$ symbol op)
    combineWith c p1 p2 = c [p1, p2]
