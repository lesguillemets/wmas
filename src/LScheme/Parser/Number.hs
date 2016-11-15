module LScheme.Parser.Number where

import LScheme.SchemeVal
import Data.Monoid
import Text.Parsec
import Text.Parsec.ByteString
import Numeric

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
