module LScheme.Parser.Float ( parseFloat )  where

import LScheme.SchemeVal
import Data.Monoid
import Text.Parsec
import Text.Parsec.ByteString
import Numeric

parseFloat :: Parser SchemeVal
parseFloat = do
    h <- try $ many1 digit <* char '.'
    b <- many digit
    let digits = h <> ('.':b)
    case readFloat digits of
         [] -> parserFail $ "cannot parse " <> digits <> " as a float"
         (x,_):_ -> return $ Float x
