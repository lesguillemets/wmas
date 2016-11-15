module LScheme where

import Prelude as P
import Data.ByteString hiding (pack)
import Data.ByteString.Char8 hiding (readInt)
import Data.Monoid
import Text.Parsec
import Text.Parsec.ByteString
import Numeric
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
charOrEscaped = escaped <|> noneOf "\\\""
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

-- TODO : must come before atom
parseNumber :: Parser SchemeVal
parseNumber = parseBinary <|> parseOctal <|> parseHexadecimal <|> parseDecimal

-- FIXME : unsafe
getReadIntResult :: String -> [(Integer,String)] -> SchemeVal
getReadIntResult name [] = error $ "readInt error on " <> name
getReadIntResult _ ((n,_):_) = Number n

parseBinary :: Parser SchemeVal
parseBinary = do
    _ <- try (string "#b")
    getReadIntResult "binary"
        . readInt 2 isZO (read.return) <$> many1 (oneOf "01")
    where
        isZO '0' = True
        isZO '1' = True
        isZO _ = False

parseOctal = do
    _ <- try (string "#o")
    getReadIntResult "octal" . readOct <$> many1 octDigit
parseHexadecimal = do
    _ <- try (string "#x")
    getReadIntResult "hexadecimal" . readHex <$> many1 hexDigit
parseDecimal = do
    _ <- try . optional $ string "#d"
    getReadIntResult "decimal" . readDec <$> many1 hexDigit


parseExpr :: Parser SchemeVal
parseExpr = parseNumber
         <|> parseString
         <|> parseAtom
