{-# LANGUAGE OverloadedStrings #-}
module LScheme.EvalSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Either
import Data.Monoid

import LScheme.Eval
import LScheme.SchemeVal
import LScheme.Internal.Testing
import LScheme.Parser

spec :: Spec
spec = do
    describe "eval" $ do
        it "leaves String as it is" $
            eval (String "HI") `shouldBe` String "HI"
        it "handles quotes" $
            eval (List [Atom "quote", Atom "that"]) `shouldBe` Atom "that"
        it "calculates (+ 2 2)" $
            eval (List [Atom "+", Number 2, Number 2]) `shouldBe` Number 4

    describe "type-testing" $ do
        it "works for symbol?" $
            typeTest "symbol?" `shouldBe` [Atom ""]
        it "works for list?" $ do
            typeTest "list?" `shouldBe` [List [], DottedList [] (Atom "")]
        it "works for integer?" $
            typeTest "integer?" `shouldBe` [Number 0]
        it "works for number?" $
            typeTest "number?" `shouldBe` [Number 0, Float 0]
        it "works for string?" $
            typeTest "string?" `shouldBe` [String ""]
        it "works for char?" $
            typeTest "char?" `shouldBe` [Character  ' ']
        it "works for boolean?" $
            typeTest "boolean?" `shouldBe` [Bool True]

-- TODO : relies on parsing, too. Where should I put the tests?
    describe "evaLoop" $ do
        it "evals 'atom as atom" $
            evPs "'atom" `shouldBe` Right (Atom "atom")
        it "currently treats (+ 2 (-4 1)) as (+ 2 0) then 2" $
            evPs "(+ 2 (-4 1))" `shouldBe` Right (Number 2)
        it "evals (+ 2 (- 4 1))" $
            evPs "(+ 2 (- 4 1))" `shouldBe` Right (Number 5)
        it "(- (+ 4 6 3) 3 5 2)" $
            evPs "(- (+ 4 6 3) 3 5 2)" `shouldBe` Right (Number 3)


    where
        typeTest test =
            map eval $
                filter (isTrue . eval . (\e -> List [Atom test, e])) vals
        vals :: [SchemeVal]
        vals = map quote [ Atom ""
                         , List []
                         , DottedList [] (Atom "")
                         , Number 0
                         , Float 0
                         , String ""
                         , Character ' '
                         , Bool True
                         ]
        quote a = List [Atom "quote", a]
        isTrue :: SchemeVal -> Bool
        isTrue (Bool True) = True
        isTrue (Bool False) = False
        evPs x = eval <$> (pt parseExpr x)
