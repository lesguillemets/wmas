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

-- TODO : relies on parsing, too. Where should I put the tests?
evaLoopSpec :: Spec
evaLoopSpec = do
    describe "evaLoop" $ do
        it "evals 'atom as atom" $
            evPs "'atom" `shouldBe` Right (Atom "atom")
        where
            evPs x = eval <$> (pt parseExpr x)

main :: IO ()
main = do
    hspec evalSpec
