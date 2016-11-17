module Main where

import LScheme
import System.IO

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    hSetBuffering stdout LineBuffering
    app
