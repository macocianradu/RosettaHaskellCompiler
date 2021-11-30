{-# LANGUAGE OverloadedStrings #-}

module PrettyPrinter.General where

import Prettyprinter
    
-- |Converts a description String into a haskell valid Doc
printDescription :: Maybe String -> Doc a -> Doc a
printDescription (Just description) doc = 
    vcat [enclose "{-" "-}" (pretty description), doc]
printDescription Nothing doc = doc