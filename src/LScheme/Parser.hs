module LScheme.Parser where

import LScheme.SchemeVal
import LScheme.Parser.Atom
import LScheme.Parser.Character
import LScheme.Parser.Number
import LScheme.Parser.String

import Text.Parsec
import Text.Parsec.ByteString

parseExpr :: Parser SchemeVal
parseExpr = parseNumber
         <|> parseCharacter
         <|> parseString
         <|> parseAtom
