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

    describe "parseExpr" $ do
        it "parses natural number" . property $
            \x -> let (x' :: Integer) = abs x in
                  num (byShow x') == Right (Number x')
        it "parses 3.3" $
            ex "3.3" `shouldBe` Right (Float 3.3)
        it "parses \"string\"" $
            ex "\"string\"" `shouldBe` Right (String "string")
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
        s = pt parseString
        a = pt parseAtom
        num = pt parseNumber
        c = pt parseCharacter
        fl = pt parseFloat
        ll = pt parseListLikes
        ex = pt parseExpr

gSS :: Gen ByteString
gSS =  liftA pack . listOf . elements $ '\t':'\r':'\n':[' '..'~']
newtype SchemeString = SchemeString { toStr :: ByteString } deriving Show
instance Arbitrary SchemeString where
    arbitrary = SchemeString <$> gSS

main :: IO ()
main = do
    hspec parseSpec
