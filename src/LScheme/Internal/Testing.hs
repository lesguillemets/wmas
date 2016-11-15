module LScheme.Internal.Testing
    ( pt
    , byShow
    , ByteString
    ) where

import Text.Parsec
import Data.ByteString.Char8 (ByteString, pack)

pt :: Parsec ByteString () r -> ByteString -> Either ParseError r
pt parser = parse parser ""

byShow :: Show a => a -> ByteString
byShow = pack . show
