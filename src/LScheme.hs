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
