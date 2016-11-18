{-# LANGUAGE OverloadedStrings #-}

module LScheme.Parser.InternalSpec where

import Test.Hspec

import LScheme.Internal.Testing

import LScheme.SchemeVal
import LScheme.Parser.Internal

spec :: Spec
spec = do
    describe "parseListLikes" $ do
        it "parses (a test)" $
            ll "(a test)" `shouldBe` Right (List [Atom "a", Atom "test"])
        it "can handle (  some  spaces  )" $
            ll "(  some  spaces  )" `shouldBe`
                Right (List [Atom "some", Atom "spaces"])
        it "parses (a  b . c)" $
            ll "(a  b . c)" `shouldBe` Right (DottedList [Atom "a", Atom "b"]
                                                       (Atom "c"))
        it "parses ( dotted .  list)" $
            ll "( dotted .  list)" `shouldBe`
                Right (DottedList [Atom "dotted"] (Atom "list"))
    where
        ll = pt parseListLikes
