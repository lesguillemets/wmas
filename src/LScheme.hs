module LScheme where

import Data.ByteString hiding (pack)
import Data.ByteString.Char8
import Text.Parsec
import Text.Parsec.ByteString
-- $setup
-- >>> :set -XOverloadedStrings

symbol :: Parsec ByteString u Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
-- |
-- >>> parseTest symbol "#there"
-- '#'

data SchemeVal = Atom ByteString
               | List [SchemeVal]
               | DottedList [SchemeVal] SchemeVal
               | Number Integer
               | String ByteString
               | Bool Bool deriving (Show, Eq)

parseString :: Parser SchemeVal
parseString = String . pack <$> (char '"' *> many charOrEscaped <* char '"')

charOrEscaped :: Parser Char
charOrEscaped = try escaped <|> noneOf "\\\""
escaped :: Parser Char
escaped = do
    -- FIXME
    _ <- char '\\'
    c <- oneOf "\\\"ntr"
    return $ case c of
                  '\\' -> c
                  '\"' -> c
                  'n' -> '\n'
                  't' -> '\t'
                  'r' -> '\r'

parseAtom :: Parser SchemeVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ case first:rest of
                  "#t" -> Bool True
                  "#f" -> Bool False
                  atom -> Atom . pack $ atom

parseNumber :: Parser SchemeVal
parseNumber = Number . read <$> many1 digit

parseExpr :: Parser SchemeVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber
