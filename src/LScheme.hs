{-# LANGUAGE OverloadedStrings #-}
module LScheme where

import qualified Data.ByteString.Char8 as BC
import Data.ByteString.Char8 (ByteString)
import Data.Monoid
import Text.Parsec hiding (spaces)
import Text.Parsec.ByteString (Parser)
import System.Environment

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String ByteString
             | Bool Bool deriving (Show)

readExpr :: ByteString -> ByteString
readExpr input = case parse (spaces1 >> symbol) "lisp" input of
                      Left err -> "No Match :" <> (BC.pack . show $ err)
                      Right val -> "Found value"


app = BC.putStrLn "Let's start over!"


