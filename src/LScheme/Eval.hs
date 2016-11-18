{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module LScheme.Eval
    ( eval
    , apply
    ) where

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
eval (List (Atom func : args)) = apply func $ map eval args

apply :: ByteString -> [SchemeVal] -> SchemeVal
apply func = fromMaybe (const (Bool False)) $ M.lookup func primitives

primitives :: M.Map ByteString ([SchemeVal] -> SchemeVal)
primitives =
    [ ("+", numericBinop (+))
    , ("-", numericBinop (-))
    , ("*", numericBinop (*))
    , ("/", numericBinop div) -- TODO: handle float
    , ("mod", numericBinop mod)
    , ("quotient", numericBinop quot)
    , ("remainder", numericBinop rem)
    , ("symbol?", unaryOp isSymbol)
    , ("integer?", unaryOp isInteger)
    , ("number?", unaryOp isNumber)
    , ("boolean?", unaryOp isBool)
    , ("string?", unaryOp isString)
    , ("list?", unaryOp isList)
    , ("char?", unaryOp isCharacter)
    ]

numericBinop :: (Integer -> Integer -> Integer) -> [SchemeVal] -> SchemeVal
numericBinop op = Number . foldl1' op . map unpackNum

-- TODO : error handling
unaryOp :: (SchemeVal -> SchemeVal) -> [SchemeVal] -> SchemeVal
unaryOp func [x] = func x
isSymbol :: SchemeVal -> SchemeVal
isSymbol (Atom _) = t
isSymbol _ = f
isList :: SchemeVal -> SchemeVal
isList (List _) = t
isList (DottedList _ _) = t
isList _ = f
-- FIXME : Number and Integer
isInteger (Number _) = t
isInteger _ = f
isNumber (Number _) = t
isNumber (Float _) = t
isNumber _ = f
isString :: SchemeVal  -> SchemeVal
isString (String _) = t
isString _ = f
isCharacter :: SchemeVal  -> SchemeVal
isCharacter (Character _) = t
isCharacter _ = f
isBool :: SchemeVal  -> SchemeVal
isBool (Bool _) = t
isBool _ = f

t = Bool True
f = Bool False

unpackNum :: SchemeVal -> Integer
unpackNum (Number n) = n
-- TODO : not liking this
unpackNum (String ns) = case reads . unpack $ ns of
                             [] -> 0
                             ((n,_):_) -> n
unpackNum (List [n]) = unpackNum n
unpackNum _ = 0 -- whoa
