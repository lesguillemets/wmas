module LScheme.Parser.Atom (parseAtom) where

import LScheme.SchemeVal
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Text.Parsec
import Text.Parsec.ByteString

parseAtom :: Parser SchemeVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    return $ case first:rest of
                  "#t" -> Bool True
                  "#f" -> Bool False
                  atom -> Atom . pack $ atom


symbol :: Parsec ByteString u Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"
