{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module LScheme.Eval where

import LScheme.SchemeVal
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
import Data.Maybe
import Data.List
import qualified Data.Map.Strict as M

eval :: SchemeVal -> SchemeVal
eval v@(String _) = v
eval v@(Float _) = v
eval v@(Number _) = v
eval v@(Bool _) = v
eval (List [Atom "quote", v]) = v
eval (List (Atom f : args)) = apply f $ map eval args

apply :: ByteString -> [SchemeVal] -> SchemeVal
apply f = fromMaybe (const (Bool False)) $ M.lookup f primitives

primitives :: M.Map ByteString ([SchemeVal] -> SchemeVal)
primitives =
    [ ("+", numericBinop (+))
    , ("-", numericBinop (-))
    , ("*", numericBinop (*))
    , ("/", numericBinop div) -- TODO: handle float
    , ("mod", numericBinop mod)
    , ("quotient", numericBinop quot)
    , ("remainder", numericBinop rem)
    ]

numericBinop :: (Integer -> Integer -> Integer) -> [SchemeVal] -> SchemeVal
numericBinop op = Number . foldl1' op . map unpackNum

unpackNum :: SchemeVal -> Integer
unpackNum (Number n) = n
-- TODO : not liking this
unpackNum (String ns) = case reads . unpack $ ns of
                             [] -> 0
                             ((n,_):_) -> n
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0 -- whoa
