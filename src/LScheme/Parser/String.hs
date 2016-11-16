module LScheme.Parser.String ( parseString )where

import LScheme.SchemeVal
import Prelude as P
import Data.ByteString.Char8 (pack)
import Text.Parsec
import Text.Parsec.ByteString

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
