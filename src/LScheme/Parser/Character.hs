module LScheme.Parser.Character
    ( parseCharacter )
    where

import LScheme.SchemeVal
import Prelude as P
import Data.Char
import Data.Monoid
import Text.Parsec
import Text.Parsec.ByteString

parseCharacter :: Parser SchemeVal
parseCharacter = do
    _ <- try $ string "#\\"
    body <- many1 anyChar
    return . Character $ case map toLower body of
                              [c] -> head body
                              "space" -> ' '
                              "newline" -> '\n'
                              -- FIXME : Error handling
                              _ -> errorHandy body
errorHandy o = error "HI"

-- errorHandy :: String -> a
-- errorHandy o = error . mconcat $
--     [ "The object \""
--     , o
--     , "\", passed as an argument to name->char, "
--     , "is not in the correct range."
--     ]
