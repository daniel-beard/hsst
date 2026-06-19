module Parser
  ( parseProgram
  ) where

import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Syntax
import Diagnostics (Span(..), noSpan)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

reservedWords :: [String]
reservedWords = ["let", "in", "true", "false"]

reserved :: String -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

identifier :: Parser Name
identifier = snd <$> identifierSpan

-- Like `identifier`, but also records the span of just the identifier token
-- (the end offset is captured before `lexeme` consumes trailing whitespace).
identifierSpan :: Parser (Span, Name)
identifierSpan = (lexeme . try) (p >>= check)
  where
    p = do
      o1 <- getOffset
      x  <- (:) <$> letterChar <*> many (alphaNumChar <|> char '_')
      o2 <- getOffset
      pure (Span o1 o2, x)
    check (sp, x)
      | x `elem` reservedWords = fail $ "keyword " ++ show x ++ " is reserved"
      | otherwise              = pure (sp, x)

intLit :: Parser Int
intLit = lexeme (L.signed (pure ()) L.decimal)

stringLit :: Parser String
stringLit = lexeme $ do
  void (char '"')
  cs <- many (escape <|> noneOf ("\"\\" :: String))
  void (char '"')
  pure cs
  where
    escape = char '\\' *> choice
      [ '\n' <$ char 'n'
      , '\t' <$ char 't'
      , '\r' <$ char 'r'
      , '\\' <$ char '\\'
      , '"'  <$ char '"'
      ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Top-level: a single expression, possibly surrounded by whitespace.
parseProgram :: String -> Either String UTerm
parseProgram src =
  case runParser (sc *> expr <* eof) "<arg>" src of
    Left err  -> Left (errorBundlePretty err)
    Right e   -> Right e

-- Lowest precedence: forward composition with |>
expr :: Parser UTerm
expr = do
  e0 <- appExpr
  rest e0
  where
    rest e =
      (do
         _  <- try (symbol "|>")
         e2 <- appExpr
         rest (compose e e2))
      <|> pure e

-- f |> g  desugars to  compose(f, g).
-- `compose` is an ordinary polymorphic primitive in the initial environment,
-- so HM gives it (a -> b) -> (b -> c) -> (a -> c) and instantiates per use-site.
compose :: UTerm -> UTerm -> UTerm
compose f g = UApp (UApp (UVar noSpan "compose") f) g

-- Application: an atom followed by zero or more `(...)` argument lists.
appExpr :: Parser UTerm
appExpr = do
  f    <- atom
  argss <- many (parens (sepBy expr (symbol ",")))
  pure (foldl applyAll f argss)
  where
    applyAll acc args = foldl UApp acc args

atom :: Parser UTerm
atom = choice
  [ lambda
  , letBinding
  , UStr  <$> stringLit
  , UInt  <$> intLit
  , UBool <$> boolLit
  , (\(sp, x) -> UVar sp x) <$> identifierSpan
  , parens expr
  ]

boolLit :: Parser Bool
boolLit = (True <$ reserved "true") <|> (False <$ reserved "false")

lambda :: Parser UTerm
lambda = do
  _  <- symbol "\\"
  xs <- some identifier
  _  <- symbol "->"
  e  <- expr
  pure (foldr ULam e xs)

letBinding :: Parser UTerm
letBinding = do
  reserved "let"
  x  <- identifier
  _  <- symbol "="
  e1 <- expr
  reserved "in"
  e2 <- expr
  pure (ULet x e1 e2)
