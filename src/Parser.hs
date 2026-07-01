module Parser
  ( parseProgram
  ) where

import Control.Monad (void)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Syntax
import Diagnostics (Span(..))

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

-- Capture the span of a raw (pre-whitespace) token parser. The end offset is
-- read before `lexeme` consumes trailing whitespace, so the span covers just the token text
spanned :: Parser a -> Parser (Span, a)
spanned p = lexeme $ do
  o1 <- getOffset
  x  <- p
  o2 <- getOffset
  pure (Span o1 o2, x)

-- The span of an operator symbol -- just the token, no trailing whitespace.
operator :: String -> Parser Span
operator s = fst <$> spanned (try (string s))

intLitSpan :: Parser (Span, Int)
intLitSpan = spanned (L.signed (pure ()) L.decimal)

stringLitSpan :: Parser (Span, String)
stringLitSpan = spanned $ do
  void (char '"')
  cs <- many (escape "\"" <|> noneOf ("\"\\" :: String))
  void (char '"')
  pure cs

charLitSpan :: Parser (Span, Char)
charLitSpan = spanned $ do
  void (char '\'')
  c <- escape "'" <|> noneOf ("'\\" :: String)
  optional (char '\'') >>= \case
    Just _  -> pure c
    Nothing -> fail
      "a Char literal must be a single unicode code point \
      \multi-code-point graphemes should use a string literal \"..\" instead"

-- Regex literal: `/{body}/`. 
-- No escape handling like strings except for `\/` (literal slash).
regexLitSpan :: Parser (Span, String)
regexLitSpan = spanned $ do
  cs <- char '/' *> many regexChar <* char '/'
  pure (concat cs)
  where
    regexChar =
          "/"               <$  try (char '\\' *> char '/')  -- `\/` -> literal `/`
      <|> (\b -> ['\\', b]) <$> (char '\\' *> anySingle)     -- keep any other `\x`
      <|> (\c -> [c])       <$> noneOf ("/\\" :: String)

-- Backslash escapes shared by string and char literals. `extra` is the
-- delimiter that also needs escaping (`"` in strings, `'` in chars).
escape :: String -> Parser Char
escape extra = char '\\' *> choice
  ( [ '\n' <$ char 'n'
    , '\t' <$ char 't'
    , '\r' <$ char 'r'
    , '\\' <$ char '\\'
    ]
    ++ [ d <$ char d | d <- extra ]
  )

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- Top-level: a single expression, possibly surrounded by whitespace.
parseProgram :: String -> Either String UTerm
parseProgram src =
  case runParser (sc *> expr <* eof) "<arg>" src of
    Left err  -> Left (errorBundlePretty err)
    Right e   -> Right e

-- Lowest precedence, left-associative: forward composition `|>` and value
-- application `&`. Both chain left-to-right, e.g. `x & f & g` is `g(f(x))`.
expr :: Parser UTerm
expr = do
  e0 <- appExpr
  rest e0
  where
    rest e =
      (do
         sp <- operator "|>"
         e2 <- appExpr
         rest (pipe sp e e2))
      <|> (do
         _  <- try (symbol "&")
         e2 <- appExpr
         rest (UApp e2 e))
      <|> pure e

-- f |> g  desugars to an application of the `|>` primitive (which behaves exactly like `compose`)
-- Distinct from `compose` allows suggesting `&` when the left operand is a value, not a function.
pipe :: Span -> UTerm -> UTerm -> UTerm
pipe sp f g = UApp (UApp (UVar sp "|>") f) g

-- Application: an atom followed by zero or more `(...)` argument lists.
appExpr :: Parser UTerm
appExpr = do
  f     <- atom
  argss <- many (parens (sepBy expr (symbol ",")))
  pure (foldl applyAll f argss)
  where
    applyAll acc args = foldl UApp acc args

atom :: Parser UTerm
atom = choice
  [ lambda
  , letBinding
  , uncurry UStr   <$> stringLitSpan
  , uncurry URegex <$> regexLitSpan
  , uncurry UChar  <$> charLitSpan
  , uncurry UInt   <$> intLitSpan
  , uncurry UBool  <$> boolLitSpan
  , uncurry UVar   <$> dollarRefSpan
  , uncurry UVar   <$> identifierSpan
  , parens expr
  ]

-- Dollar ref, like `$1`, `$2`
-- Resolves to most recent capture groups
dollarRefSpan :: Parser (Span, Name)
dollarRefSpan = spanned $ do
  void (char '$')
  n <- some digitChar
  pure ('$' : n)

boolLitSpan :: Parser (Span, Bool)
boolLitSpan = spanned $
  ((True <$ string "true") <|> (False <$ string "false")) <* notFollowedBy alphaNumChar

lambda :: Parser UTerm
lambda = do
  xs <- symbol "\\" *> some identifier <* symbol "->"
  e  <- expr
  pure (foldr ULam e xs)

letBinding :: Parser UTerm
letBinding = do
  reserved "let"
  x  <- identifier
  _  <- symbol "="
  e1 <- expr
  reserved "in"
  ULet x e1 <$> expr
