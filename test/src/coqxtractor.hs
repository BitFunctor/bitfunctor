module Main where

import Network.BitFunctor.Theory.Types
import qualified Network.BitFunctor.Theory.Coq.StatementExtraction as SE
import qualified Network.BitFunctor.Common as Common
import Network.BitFunctor.Crypto.Hash.Types
import qualified Network.BitFunctor.Crypto.Hash as Hash
import Network.BitFunctor.Theory.Types
import qualified Network.BitFunctor.Theory.Coq.Constants as Constants
import qualified Network.BitFunctor.Identifiable as Ident
import System.Environment
import qualified Data.Map as Map
import qualified Data.List as List
import Data.List.Split (splitOneOf)
import Control.Monad
import Data.Text.IO as DTIO (writeFile)
import qualified Data.Time as Time (getCurrentTime)

commandLineFlags = ["-e"]

-- t a -> (a -> m b) -> m (t b) 
main = do
       args <- getArgs
       let argsSplitted = splitOneOf commandLineFlags args
       let libs = Common.headWithDefault [] argsSplitted
       extractedStms <- SE.extractStatements libs
       -- putStrLn $ show extractedStms
       let us = List.nub $ concat $ List.map stuses extractedStms
       let th = Map.fromList $ List.map (\s -> (stname s, s)) extractedStms
       forM_ us (\u -> if (fst u /= SelfReference) && (fst u /= BoundVariable) then
                              if (Map.member (snd u) th) then
                                    putStrLn $ "!Found: " ++ (show u)
                              else
                                    putStrLn $ "?Found: " ++ (show u)
                        else return ())
       putStrLn "Libraries have been processed, extracting terms..."
       let terms = Common.headWithDefault [] $ Common.tailWithDefault [[]] argsSplitted
       -- putStrLn $ show terms
       let extractedCodes = Prelude.concat $ SE.extractTermsCode terms extractedStms 
       putStrLn $ "Totally extracted: " ++ (show $ Prelude.length extractedCodes)
       forM_ extractedCodes (\t -> do
                                     date <- Time.getCurrentTime -- "2008-04-18 14:11:22.476894 UTC"
                                     let sename = "Ex" ++ Constants.generatedFilePrefix ++ (toString $ Ident.id $ Ident.ByBinary $ show date) ++ Constants.vernacFileSuffix 
                                     DTIO.writeFile sename t)
       putStrLn "Terms have been processed"
