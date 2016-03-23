module Main where

import Network.BitFunctor.Theory.Types
import qualified Network.BitFunctor.Theory.Coq.StatementExtraction as SE
import System.Environment
import qualified Data.Map as Map
import qualified Data.List as DL 
import Control.Monad

-- t a -> (a -> m b) -> m (t b) 
main = do
       args <- getArgs
       extractedStms <- SE.extractStatements args [] []
       putStrLn $ show extractedStms
       let us = DL.nub $ concat $ DL.map uses extractedStms
       let th = Map.fromList $ DL.map (\s -> (name s, s)) extractedStms
       forM_ us (\u -> if (Map.member (snd u) th) then
                           putStrLn $ "!Found: " ++ (show u)
                       else
                           putStrLn $ "?Found: " ++ (show u))
