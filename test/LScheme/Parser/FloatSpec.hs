{-# LANGUAGE OverloadedStrings #-}

module LScheme.Parser.FloatSpec where

import Test.Hspec
import Test.QuickCheck

import LScheme.Internal.Testing

import LScheme.SchemeVal
import LScheme.Parser.Float

spec :: Spec
spec = do
    describe "parseFloat" $ do
        it "parses 14.32" $
            fl "14.32" `shouldBe` Right (Float 14.32)
        it "parses 14." $
            fl "14." `shouldBe` Right (Float 14)
--         it "parses random double" . property $
--             \x -> let (x' :: Double) = abs x in
--                     fl (byShow x') == Right (Float x')
--         no support for d.de2 thing
    where
        fl = pt parseFloat
