{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Test.Hspec
import Test.QuickCheck
import Data.Either
import Data.Monoid
import Data.Char (intToDigit)
import Numeric
import Control.Applicative (liftA)
import LScheme
import LScheme.Parser.Atom
import LScheme.Parser.String
import LScheme.Parser.Number
import LScheme.Internal.Testing

parseSpec:: Spec
parseSpec = do
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
                        in s (byShow str) `shouldBe` Right (String str)
        it "doesn't parse invalid escape: \\g" $
            s "\"invalid \\g\"" `shouldSatisfy` isLeft

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

    describe "parseNumber" $ do
        it "parses natural number" . property $
            \x -> let (x' :: Integer) = abs x in
                  num (byShow x') == Right (Number x')
        it "parses binary" . property $
            \n -> let (n':: Integer) = abs n in
                  num (pack $ "#b" <> showIntAtBase 2 intToDigit n' "")
                      `shouldBe` Right (Number n')
        it "parses octal" . property $
            \n -> let (n':: Integer) = abs n in
                  num (pack $ "#o" <> showOct n' "")
                     `shouldBe` Right (Number n')
        it "parses hex" . property $
            \n -> let (n':: Integer) = abs n in
                  num (pack $ "#x" <> showHex n' "")
                    `shouldBe` Right (Number n')
        it "parses decimal with #d" . property $
            \n -> let (n':: Integer) = abs n in
                  num (pack $ "#d" <> show n') `shouldBe` Right (Number n')



    where
        s = pt parseString
        a = pt parseAtom
        num = pt parseNumber

gSS :: Gen ByteString
gSS =  liftA pack . listOf . elements $ '\t':'\r':'\n':[' '..'~']
newtype SchemeString = SchemeString { toStr :: ByteString } deriving Show
instance Arbitrary SchemeString where
    arbitrary = SchemeString <$> gSS

main :: IO ()
main = do
    hspec parseSpec
