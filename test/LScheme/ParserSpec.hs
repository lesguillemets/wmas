{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LScheme.ParserSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Either

import LScheme.Internal.Testing

import LScheme.SchemeVal
import LScheme.Parser

spec :: Spec
spec = do
    describe "parseExpr" $ do
        it "parses 3.3" $
            ex "3.3" `shouldBe` Right (Float 3.3)
        it "parses \"string\"" $
            ex "\"string\"" `shouldBe` Right (String "string")
        it "parses #t" $
            ex "#t" `shouldBe` Right (Bool True)
        it "parses #\\a" $
            ex "#\\a" `shouldBe` Right (Character 'a')
        it "fails for #inv" $
            ex "'#inv" `shouldSatisfy` isLeft
        it "parses (a test)" $
            ex "(a test)" `shouldBe` Right (List [Atom "a",Atom "test"])
        it "parses ( a test  )" $
            ex "( a test  )" `shouldBe` Right (List [Atom "a",Atom "test"])
        it "parses (a (nested) list)" $
            ex "(a (nested) list))"
                `shouldBe`
                Right ( List [ Atom "a"
                             , List [Atom "nested"]
                             , Atom "list"])
        it "parses (a (dotted . list) test)" $
            ex "(a (dotted . list) test)" `shouldBe`
                Right (List [ Atom "a"
                            , DottedList [Atom "dotted"] (Atom "list")
                            , Atom "test"])
        it "parses (a '(quoted (dotted . list)) test)" $
            ex "(a '(quoted (dotted . list)) test)" `shouldBe`
                Right (List [ Atom "a"
                            , List [ Atom "quote"
                                   , List [ Atom "quoted"
                                          , DottedList [Atom "dotted"]
                                                       (Atom "list")
                                          ]
                                   ]
                            , Atom "test"])
        it "fails on (a '(imbalanced parens)" $
            ex "(a '(imbalanced parens)" `shouldSatisfy` isLeft
        -- ?
        it "halts without error" . property $
            \str -> ex (pack str) `shouldSatisfy` (const True)
    where
        ex = pt parseExpr
