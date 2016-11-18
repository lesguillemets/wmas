{-# LANGUAGE OverloadedStrings #-}

module LScheme.Parser.StringSpec where

import Test.Hspec
import Test.QuickCheck
import Data.Either
import Control.Applicative

import LScheme.Internal.Testing

import LScheme.SchemeVal
import LScheme.Parser.String

spec :: Spec
spec = do
    describe "parseString" $ do
        it "parses normal string" $
            s "\"There\"" `shouldBe` Right (String "There")
        it "handles escaped backslash" $
            s (byShow "\\") `shouldBe` Right (String "\\")
        it "handles escaped quotes" $
            s (byShow "say \"hi\"") `shouldBe` Right (String "say \"hi\"")
        it "handles \\n" $
            s (byShow "foo \n bar") `shouldBe` Right (String "foo \n bar")
        it "handles random string" . property $
            \inp -> let str = toStr inp
                        in s (byShow str) == Right (String str)
        it "doesn't parse invalid escape: \\g" $
            s "\"invalid \\g\"" `shouldSatisfy` isLeft
    where
        s = pt parseString

newtype SchemeString = SchemeString { toStr :: ByteString } deriving Show
instance Arbitrary SchemeString where
    arbitrary = SchemeString <$> gSS

gSS :: Gen ByteString
gSS =  liftA pack . listOf . elements $ '\t':'\r':'\n':[' '..'~']

