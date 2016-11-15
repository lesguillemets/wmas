{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Data.Either (rights)
import LScheme
import LScheme.Internal.Testing

parseStringSpec :: Spec
parseStringSpec = do
    describe "parseString" $ do
        it "parses normal string" $
            p "\"There\"" `shouldBe` Right (String "There")
    where p = pt parseString

main :: IO ()
main = do
    hspec parseStringSpec
