{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Test.Hspec
import Test.QuickCheck
import Data.Either
import Data.Monoid
import Data.Char (intToDigit)
import Control.Exception (evaluate)
import Numeric
import Control.Applicative (liftA)
import LScheme
import LScheme.Parser.Internal
import LScheme.Parser.Float
import LScheme.Parser.Character
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
                        in s (byShow str) == Right (String str)
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
                      == Right (Number n')
        it "parses octal" . property $
            \n -> let (n':: Integer) = abs n in
                  num (pack $ "#o" <> showOct n' "") == Right (Number n')
        it "parses hex" . property $
            \n -> let (n':: Integer) = abs n in
                  num (pack $ "#x" <> showHex n' "") == Right (Number n')
        it "parses decimal with #d" . property $
            \n -> let (n':: Integer) = abs n in
                  num (pack $ "#d" <> show n') == Right (Number n')

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

    describe "parseFloat" $ do
        it "parses 14.32" $
            fl "14.32" `shouldBe` Right (Float 14.32)
        it "parses 14." $
            fl "14." `shouldBe` Right (Float 14)
--         it "parses random double" . property $
--             \x -> let (x' :: Double) = abs x in
--                     fl (byShow x') == Right (Float x')
--         no support for d.de2 thing
    describe "parseExpr" $ do
        it "parses (a test)" $
            ex "(a test)" `shouldBe` Right (List [Atom "a",Atom "test"])
    where
        s = pt parseString
        a = pt parseAtom
        num = pt parseNumber
        c = pt parseCharacter
        fl = pt parseFloat
        li = pt parseList
        ex = pt parseExpr

gSS :: Gen ByteString
gSS =  liftA pack . listOf . elements $ '\t':'\r':'\n':[' '..'~']
newtype SchemeString = SchemeString { toStr :: ByteString } deriving Show
instance Arbitrary SchemeString where
    arbitrary = SchemeString <$> gSS

main :: IO ()
main = do
    hspec parseSpec
