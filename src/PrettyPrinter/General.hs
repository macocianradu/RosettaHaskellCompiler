{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinter.General where

import Prettyprinter
    
printDescription :: Maybe String -> Doc a -> Doc a
printDescription (Just description) doc = 
    vcat [enclose "{-" "-}" (pretty description), doc]
printDescription Nothing doc = doc