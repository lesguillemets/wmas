{-# LANGUAGE OverloadedStrings #-}
module LScheme where

import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Char8 (ByteString, pack)
import Data.Monoid
import Text.Parsec hiding (spaces)
import Text.Parsec.ByteString (Parser)
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space

data LispVal = Atom ByteString
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String ByteString
             | Bool Bool deriving (Show)

parseString :: Parser LispVal
parseString = (String . pack) <$> (char '"' *> many (noneOf "\"") <* char '"')

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = pack $ first : rest
    return $ case atom of
                  "#t" -> Bool True
                  "#f" -> Bool False
                  _ -> Atom atom


parseNumber :: Parser LispVal
parseNumber = (Number . read) <$> many1 digit

parseList :: Parser LispVal
parseList = List <$> (parseExpr `sepBy` spaces1)

parseDottedList :: Parser LispVal
parseDottedList = do
    heads <- parseExpr `endBy` spaces1
    tail <- char '.' *> spaces1 *> parseExpr
    return $ DottedList heads tail

parseQuoted :: Parser LispVal
parseQuoted = do
    x <- char '\'' *> parseExpr
    return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber <|> parseQuoted
         <|> do char '(' *> (try parseList <|> parseDottedList) <* char ')'

readExpr :: ByteString -> ByteString
readExpr input = case parse parseExpr "lisp" input of
                      Left err -> "No Match :" <> (BC.pack . show $ err)
                      Right val -> "Found value"

app = BC.putStrLn "Let's start over!"


