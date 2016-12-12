{-# LANGUAGE OverloadedStrings #-}

module Network.BitFunctor.Theory.Coq.Extraction.TheoryExtraction  where

import qualified Data.Text as Text
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (catMaybes)

import Network.BitFunctor.Theory.Types
import qualified Network.BitFunctor.Common as Common

unfoldUses :: TheoryC a k c c' s t => [s] -> [s] -> t -> IO [s]
unfoldUses [] acct _ = return acct
unfoldUses (st:sts) acct th = do
                         putStrLn $ "Unfolding " ++ (Text.unpack $ toSuffix st) ++ ", remaining " ++ (show $ List.length sts)
                         let thm = toStatementMap th
                         let b = List.notElem st acct 
                         -- let isExtractable u = (fst u /= SelfReference) && (fst u /= BoundVariable) && (fst u /= LocalConstructor)
                         let stc = toStatementCode st
                         case stc of
                           Left _ -> do
                                      putStrLn "Statement code is not parsed, skipping..."
                                      unfoldUses sts acct thm
                           Right cl -> let usm = catMaybes $ List.map (\u -> case u of
                                                                         Left _ -> Nothing
                                                                         Right e -> if isTheoriable e then
                                                                                       Map.lookup (toKey $ toKey e) thm
                                                                                    else Nothing) cl in
                                       if b then                             
                                         unfoldUses (usm ++ sts) (st:acct) th
                                       else do
                                            putStrLn "skipping..."
                                            unfoldUses sts acct th

unfoldUsesList :: TheoryC a k c c' s t => [s] -> t -> IO [[s]]
unfoldUsesList sts th = mapM (\s -> unfoldUses [s] [] th) sts 

extractTerms :: TheoryC a k c c' s t => t -> [Text.Text] -> IO [[[s]]]
extractTerms _ []  = return []
extractTerms th (t:ts) = do
                              et <- do
                                  let fsts = List.filter (\s -> (toStatementName s == t)) $ toStatementList th
                                  r <- unfoldUsesList fsts th 
                                  return $ List.map (Common.partsort toStatementName partCompare) r                                  
                              r <- extractTerms th ts 
                              return $ et:r

extractTermsCode :: [Text.Text] -> [PreStatementWithList] -> IO [[Text]]
extractTermsCode ts sts = do
                          extss <- extractTerms ts sts
                          putStrLn $ "Length of extracted terms: " ++ (show $ List.map (List.map (\ext -> List.length ext)) extss)
                          return $ List.map (List.map (\ext -> DT.concat $ List.map (\s -> (fromCode $ stcode s) <> "\n(* end " <> (fqStatementName $ stname s) <> " *)\n\n") ext)) extss
