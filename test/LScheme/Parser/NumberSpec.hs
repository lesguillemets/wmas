{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LScheme.Parser.NumberSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Char (intToDigit)
import Data.Monoid
import Numeric

import LScheme.Internal.Testing

import LScheme.SchemeVal
import LScheme.Parser.Number

spec :: Spec
spec = do
    describe "parseNumber" $ do
        it "parses natural number" . property $
            \x -> let (x' :: Integer) = abs x in
                  num (byShow x') == Right (Number x')
        it "parses binary" . property $
            \n -> let (n':: Integer) = abs n in
                  num (pack $ "#b" <> showIntAtBase 2 intToDigit n' "")
                      == Right (Number n')
        it "parses octal" . property $
            \n -> let (n':: Integer) = abs n in
                  num (pack $ "#o" <> showOct n' "") == Right (Number n')
        it "parses hex" . property $
            \n -> let (n':: Integer) = abs n in
                  num (pack $ "#x" <> showHex n' "") == Right (Number n')
        it "parses decimal with #d" . property $
            \n -> let (n':: Integer) = abs n in
                  num (pack $ "#d" <> show n') == Right (Number n')
    where
        num = pt parseNumber
