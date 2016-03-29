module Main where

import Network.BitFunctor.Theory.Types
import qualified Network.BitFunctor.Theory.Coq.StatementExtraction as SE
import System.Environment
import qualified Data.Map as Map
import qualified Data.List as List 
import Control.Monad

-- t a -> (a -> m b) -> m (t b) 
main = do
       args <- getArgs
       extractedStms <- SE.extractStatements args [] []
       putStrLn $ show extractedStms
       let us = List.nub $ concat $ List.map stuses extractedStms
       let th = Map.fromList $ List.map (\s -> (stname s, s)) extractedStms
       forM_ us (\u -> if (Map.member (snd u) th) then
                           putStrLn $ "!Found: " ++ (show u)
                       else
                           putStrLn $ "?Found: " ++ (show u))
