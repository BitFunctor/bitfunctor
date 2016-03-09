module Main where

import qualified StatementExtraction as SE
import System.Environment


main = do
       args <- getArgs
       extractedStms <- SE.extractStatements args []
       putStrLn $ show extractedStms
