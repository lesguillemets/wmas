{-# LANGUAGE OverloadedStrings #-}
module LScheme.Eval where

import LScheme.SchemeVal

eval :: SchemeVal -> SchemeVal
eval v@(String _) = v
eval v@(Float _) = v
eval v@(Number _) = v
eval v@(Bool _) = v
eval (List [Atom "quote", v]) = v
