{-# LANGUAGE OverloadedStrings #-}

module LScheme.Parser.CharacterSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Either
import Data.Monoid
import LScheme.Internal.Testing

import LScheme.SchemeVal
import LScheme.Parser.Character

spec :: Spec
spec = do
    describe "parseCharacter" $ do
        it "parses #\\a" $
            c "#\\a" `shouldBe` Right (Character 'a')
        it "parses arbitrary character" . property $
            \ch -> c (pack . ("#\\" <>) . return $ ch) == Right (Character ch)
        it "parses  #\\space" $
            c "#\\space" `shouldBe` Right (Character ' ')
        it "parses  #\\newline" $
            c "#\\newline" `shouldBe` Right (Character '\n')
        it "is case-insensitive for #\\<character name> notation" $
            let cases = allCases "space"
                in
                map (c . pack . ("#\\" <>)) cases
                    `shouldSatisfy` all (== Right (Character ' '))
        it "doesn't parse unrecognized names" $
            (c "#\\invalid") `shouldSatisfy` isLeft
    where
        c = pt parseCharacter
