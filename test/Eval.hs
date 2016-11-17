{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Test.QuickCheck
import Data.Either
import Data.Monoid

import LScheme.Eval
import LScheme.SchemeVal
import LScheme.Internal.Testing
import LScheme.Parser

evalSpec :: Spec
evalSpec = do
    describe "eval" $ do
        it "leaves String as it is" $
            eval (String "HI") `shouldBe` String "HI"
        it "handles quotes" $
            eval (List [Atom "quote", Atom "that"]) `shouldBe` Atom "that"
        it "calculates (+ 2 2)" $
            eval (List [Atom "+", Number 2, Number 2]) `shouldBe` Number 4

-- TODO : relies on parsing, too. Where should I put the tests?
evaLoopSpec :: Spec
evaLoopSpec = do
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
            evPs x = eval <$> (pt parseExpr x)

main :: IO ()
main = do
    hspec evalSpec
