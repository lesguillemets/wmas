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
parseString = String . pack <$> (char '"' *> many (noneOf "\"") <* char '"')

parseAtom :: Parser SchemeVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ case first:rest of
                  "#t" -> Bool True
                  "#f" -> Bool False
                  atom -> Atom . pack $ atom
