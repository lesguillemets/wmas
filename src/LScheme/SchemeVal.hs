{-# LANGUAGE OverloadedStrings #-}
module LScheme.SchemeVal
    ( SchemeVal (..)
    , showVal
    )
    where

import Prelude hiding (unwords)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unwords, pack)
import Data.Monoid

data SchemeVal = Atom ByteString
               | List [SchemeVal]
               | DottedList [SchemeVal] SchemeVal
               | Number Integer
               | Float Double
               | String ByteString
               | Character Char
               | Bool Bool deriving (Show, Eq)

showVal :: SchemeVal -> ByteString
showVal (Atom name) = name
showVal (List ls) = "(" <> (unwords . map showVal $ ls) <> ")"
showVal (DottedList h t) =
    "(" <> (unwords . map showVal $ h) <> " . " <> showVal t <> ")"
showVal (Number n) = pack . show $ n
showVal (Float x) = pack . show $ x
showVal (String s) = s
showVal (Character c) = "#\\" <> (pack . return $ c)
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
