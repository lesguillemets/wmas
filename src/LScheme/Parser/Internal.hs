{-# LANGUAGE OverloadedStrings #-}
module LScheme.Parser.Internal
    ( parseExpr
    , parseListLikes
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
         <|> parseFloat -- TODO : being order-dependent
         <|> parseNumber
         <|> parseQuoted
         <|> parseListLikes

-- parses List and DottedList.
parseListLikes :: Parser SchemeVal
parseListLikes = do
    _ <- char '(' <* spaces
    pre <- parseListsPre <* spaces
    afterDot <- optionMaybe $ char '.' *> spaces1 *> parseExpr
    _ <- spaces *> char ')'
    return $ listOrDotted pre afterDot
    where
        listOrDotted :: [SchemeVal] -> Maybe SchemeVal -> SchemeVal
        listOrDotted pre Nothing = List pre
        listOrDotted l (Just d) = DottedList l d

parseListsPre :: Parser [SchemeVal]
parseListsPre = parseExpr `sepEndBy` spaces1 -- END!

parseQuoted :: Parser SchemeVal
parseQuoted = do
    quoted <- char '\'' *> parseExpr
    return $ List [Atom "quote", quoted]

spaces1 :: Parser ()
spaces1 = skipMany1 space

-- depreceated

parseList :: Parser SchemeVal
parseList = List <$> parseExpr `sepBy` spaces1

parseDottedList :: Parser SchemeVal
parseDottedList = do
    dottedHead <- parseExpr `endBy1` spaces1
    dottedTail <- char '.' *> spaces1 *> parseExpr
    return $ DottedList dottedHead dottedTail
