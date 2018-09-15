{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.CSV where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L
import           Data.CSV.Lexer

-- | test.csv
testCSV = "1,2,3,4,5\n1,2,3,4,5\n"

example :: IO ()
example = mapM_ print (getCSV testCSV)
