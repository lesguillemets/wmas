module LScheme.Parser.Character
    ( parseCharacter )
    where

import LScheme.SchemeVal
import Prelude as P
import Data.Char
import Text.Parsec
import Text.Parsec.ByteString

parseCharacter :: Parser SchemeVal
parseCharacter = do
    _ <- try $ string "#\\"
    body <- many1 anyChar
    -- TODO : return being verbose
    Character <$> case map toLower body of
                       [c] -> return $ head body
                       "space" -> return ' '
                       "newline" -> return '\n'
                       _ -> fail $ errorHandy body -- FIXME: not showing up?
errorHandy :: String -> String
errorHandy o = mconcat [ "The object \""
                       , o
                       , "\", passed as an argument to name->char, "
                       , "is not in the correct range."
                       ]
