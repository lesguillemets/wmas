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
         <|> (do
                char '(' *>
                    try parseList <|> parseDottedList
                <* char ')')

parseList :: Parser SchemeVal
parseList = List <$> parseExpr `sepBy` spaces

parseDottedList :: Parser SchemeVal
parseDottedList = do
    head <- parseExpr `endBy` spaces
    tail <- char '.' *> spaces *> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser SchemeVal
parseQuoted = do
    quoted <- char '\\' *> parseExpr
    return $ List [Atom "quote", quoted]
