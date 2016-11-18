{-# LANGUAGE OverloadedStrings #-}

module LScheme.Parser.AtomSpec where

import Test.Hspec
import Data.Either

import LScheme.Internal.Testing

import LScheme.SchemeVal
import LScheme.Parser.Atom

spec :: Spec
spec = do
    describe "parseAtom" $ do
        it "parses '#t'" $
            a "#t" `shouldBe` Right (Bool True)
        it "parses '#f'" $
            a "#f" `shouldBe` Right (Bool False)
        it "parses ':a'" $
            a ":a" `shouldBe` Right (Atom ":a")
        it "parses 'atom'" $
            a "atom" `shouldBe` Right (Atom "atom")
        it "doesn't parse '1a'" $
            a "1a" `shouldNotSatisfy` isRight
    where
        a = pt parseAtom


