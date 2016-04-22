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
import qualified Data.Serialize as DS (decode, encode)
import qualified Data.ByteString as BS (writeFile, readFile)

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
       forM_ us (\u -> if (fst u /= SelfReference) && (fst u /= BoundVariable) && (fst u /= LocalConstructor) then
                              if (Map.member (snd u) th) then
                                    putStrLn $ "!Found: " ++ (show u)
                              else
                                    putStrLn $ "?Found: " ++ (show u)
                        else return ())
       {--
       putStrLn "Checking for cycles..."
       forM_ extractedStms (\s -> do
                                     putStrLn $ "Checking " ++ (show $ stname s)
                                     forM_ (stuses s) (\u -> 
                                                              if (fst u /= SelfReference) && (fst u /= BoundVariable) && (fst u /= LocalConstructor) then do
                                                                let ms' = Map.lookup (snd u) th
                                                                case ms' of
                                                                 Nothing -> putStrLn $ "   Dependence " ++ (show $ snd u) ++ " not found"
                                                                 Just s' -> if (List.elem (stname s) $ List.map snd $ stuses s') then
                                                                               putStrLn $ "   Dependence " ++ (show $ snd u) ++ " form a cycle"
                                                                            else
                                                                               putStrLn $ "   Dependence " ++ (show $ snd u) ++ " is OK"  
                                                               else putStrLn $ "   Dependence " ++ (show $ snd u) ++ " is unextractable"))
       --}

       putStrLn "Libraries have been processed, extracting terms..."
       {--
       putStrLn "Writing theory to the file..."
       let bsw = DS.encode extractedStms
       BS.writeFile "theory.btf" bsw
       putStrLn "Reading theory from the file..."
       bsr <- BS.readFile "theory.btf"
       let rthl = DS.decode bsr
       case rthl of
         Left s -> putStrLn $  "Error in decoding: " ++ s
         Right th ->  if (th /= extractedStms) then
                        putStrLn "Writing/reading fails"
                      else
                        putStrLn "Writing/reading is OK"
       --}
       let terms = Common.headWithDefault [] $ Common.tailWithDefault [[]] argsSplitted
       -- putStrLn $ show terms
       ec <- SE.extractTermsCode terms extractedStms
       let extractedCodes = Prelude.concat ec
       -- putStrLn $ "Totally extracted: " ++ (show $ Prelude.length extractedCodes)
       forM_ extractedCodes (\t -> do
                                     date <- Time.getCurrentTime -- "2008-04-18 14:11:22.476894 UTC"
                                     let sename = "Ex" ++ Constants.generatedFilePrefix ++ (toString $ Ident.id $ Ident.ByBinary $ show date) ++ Constants.vernacFileSuffix 
                                     DTIO.writeFile sename t)
       putStrLn "Terms have been processed"
