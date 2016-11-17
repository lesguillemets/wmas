{-# LANGUAGE OverloadedStrings #-}
module LScheme
    ( module LScheme.SchemeVal
    , module LScheme.Parser
    , app
    ) where

import LScheme.SchemeVal
import LScheme.Parser

import Prelude hiding (getLine, putStrLn, null)
import Text.Parsec
import Data.ByteString hiding (pack)
import Data.ByteString.Char8 (pack)
import Data.Monoid

app :: IO ()
app = do
    l <- getLine
    if null l
       then return ()
       else (putStrLn . readExpr)  l >> app

readExpr :: ByteString -> ByteString
readExpr input =
    case parse parseExpr "scheme" input of
         Left err -> pack . show $ err
         Right val -> showVal val
