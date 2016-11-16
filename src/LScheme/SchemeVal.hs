module LScheme.SchemeVal where

import Data.ByteString (ByteString)

data SchemeVal = Atom ByteString
               | List [SchemeVal]
               | DottedList [SchemeVal] SchemeVal
               | Number Integer
               | Float Double
               | String ByteString
               | Character Char
               | Bool Bool deriving (Show, Eq)
