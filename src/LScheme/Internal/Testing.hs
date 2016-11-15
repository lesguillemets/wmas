module LScheme.Internal.Testing
    ( pt
    , byShow
    , ByteString
    , pack
    , allCases
    ) where

import Text.Parsec
import Data.ByteString.Char8 (ByteString, pack)
import Data.Char

pt :: Parsec ByteString () r -> ByteString -> Either ParseError r
pt parser = parse parser ""

byShow :: Show a => a -> ByteString
byShow = pack . show

allCases :: String -> [String]
allCases = foldr (\c acc -> map (toLower c:) acc ++ map (toUpper c:) acc ) [""]
-- | like this.
-- >>> allCases "acc"
-- ["acc","acC","aCc","aCC","Acc","AcC","ACc","ACC"]
