{-# LANGUAGE OverloadedStrings #-}
module LScheme.Parser.Internal
    ( parseExpr
    , parseList
    , parseDottedList
    , parseQuoted
    ) where
import LScheme.SchemeVal
import LScheme.Parser.Atom
import LScheme.Parser.Character
import LScheme.Parser.Number
import LScheme.Parser.Float
import LScheme.Parser.String

import Text.Parsec
import Text.Parsec.ByteString

parseExpr :: Parser SchemeVal
parseExpr = parseAtom
         <|> parseCharacter
         <|> parseString
         <|> parseNumber
         <|> parseFloat
         <|> parseQuoted
         <|> (char '(' *> (try parseList <|> parseDottedList) <* char ')')

parseList :: Parser SchemeVal
parseList = List <$> parseExpr `sepBy` spaces1

parseDottedList :: Parser SchemeVal
parseDottedList = do
    dottedHead <- parseExpr `endBy1` spaces1
    dottedTail <- char '.' *> spaces1 *> parseExpr
    return $ DottedList dottedHead dottedTail

parseQuoted :: Parser SchemeVal
parseQuoted = do
    quoted <- char '\'' *> parseExpr
    return $ List [Atom "quote", quoted]

spaces1 :: Parser ()
spaces1 = skipMany1 space
