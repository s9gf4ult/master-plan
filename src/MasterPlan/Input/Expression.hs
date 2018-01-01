module MasterPlan.Input.Expression where

import Text.Megaparsec
import MasterPlan.Algebra

-- |Parses the part of right-hand-side after the optional properties
--  (literal string title or properties between curly brackets)
expression ∷ ProjectProperties -> Parser (Algebra ProjectKey)
expression props =
    simplify <$> makeExprParser term table <?> "expression"
  where
    term =  parens (expression defaultProjectProps) <|> (Annotated <$> projectKey)
    table = [[binary "*" (combineWith Product)]
            ,[binary "->" (combineWith Sequence)]
            ,[binary "+" (combineWith Sum)]]
    binary  op f = InfixL (f <$ symbol op)

    combineWith c p1 p2 = c props $ p1 NE.<| [p2]
