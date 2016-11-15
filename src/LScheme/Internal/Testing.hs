module LScheme.Internal.Testing
    ( pt
    , ByteString
    ) where

import Text.Parsec
import Data.ByteString (ByteString)

pt :: Parsec ByteString () r -> ByteString -> Either ParseError r
pt parser = parse parser ""
