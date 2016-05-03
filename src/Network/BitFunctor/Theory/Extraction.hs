{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE ScopedTypeVariables, TypeOperators, GADTs, FlexibleInstances #-}

module Network.BitFunctor.Theory.Extraction where

import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Monoid ((<>))
import Data.Maybe (catMaybes)
import Control.Monad
import Control.Lens

import Network.BitFunctor.Theory.Types
import qualified Network.BitFunctor.Common as Common
import qualified Network.BitFunctor.Theory.Utils as Utils

eqCodeParts :: TheoryC a k c c' s t => t -> [Either c c'] -> [Either c c'] -> Bool
eqCodeParts _ [] [] = True
eqCodeParts th ((Left t1):pcp1) ((Left t2):pcp2) = if (toText t1 == toText t2) then (eqCodeParts th pcp1 pcp2) else False
eqCodeParts th ((Right e1):pcp1) ((Right e2):pcp2) =
                          -- shortname should be at least equal or that is self-reference
                          let b1 = (toSuffix e1 == toSuffix e2) || (isSelfReference e1) && (isSelfReference e2) in
                          -- if both "extractable" extract and compare
                          if (isTheoriable e1) && (isTheoriable e2) then 
                              let ms1 = Map.lookup (toKey $ toKey e1) $ toStatementMap th in
                              let ms2 = Map.lookup (toKey $ toKey e2) $ toStatementMap th in
                              case (ms1, ms2) of
                                   (Nothing, _) -> if b1 then (eqCodeParts th pcp1 pcp2) else False
                                   (_, Nothing) -> if b1 then (eqCodeParts th pcp1 pcp2) else False
                                   (Just s1, Just s2) -> if (b1 || eqStatementCode th s1 s2) then (eqCodeParts th pcp1 pcp2) else False
                          else
                          -- if something unextractable, e.g. LocalConstructor, only shortname should be equal ???
                              if b1 then (eqCodeParts th pcp1 pcp2) else False
eqCodeParts _ _ _ = False


eqCode :: TheoryC a k c c' s t => t -> CodeA c c' -> CodeA c c' -> Bool
eqCode _ (Left _) _ = False
eqCode _ _ (Left _) = False
eqCode th (Right p1) (Right p2) = eqCodeParts th p1 p2

eqStatementCode :: TheoryC a k c c' s t => t -> s -> s -> Bool
eqStatementCode th s1 s2 = let c1 = toStatementCode s1 in
                           let c2 = toStatementCode s2 in
                           case (c1, c2) of
                             -- never be called
                             -- (Left _, Left _) -> False
                             -- only for Constructors, should not be called for local constructors, when comparing Inductives
                             -- so only one-typed constructors will be equal
                             (Right [], Right []) -> (toSuffix $ toStatementName s1) == (toSuffix $ toStatementName s2)
                             (_, _) -> eqCode th c1 c2



changeCodeReference :: (Nameable b n) => n -> n -> CodeA a b -> CodeA a b
changeCodeReference _ _ c@(Left _) = c
changeCodeReference s' s (Right ps) = Right $ List.map (\p -> case p of
                                                                Left t -> Left t
                                                                Right e -> Right $ changeNameWithKey s' s e) ps

-- changeUsesReference :: CoqStatementName -> CoqStatementName -> CoqTerm -> CoqTerm
-- changeUsesReference s' s u = if (snd u == s') then (fst u, s) else u

changeStatementReference :: (StatementC a k c c' s) => a -> a -> s -> s
changeStatementReference n' n st = changeStatementCode (changeCodeReference n' n $ toStatementCode st) st
-- v { -- stuses = List.map (changeUsesReference s' s) $ stuses v,
--                                      stcode = changeCodeReference s' s $ stcode v}

collapseStatementGroups :: (StatementC a k c c' s) => [[s]] -> [s] -> [s]
collapseStatementGroups [] acc = acc
collapseStatementGroups ([]:gs) acc = collapseStatementGroups gs acc
collapseStatementGroups ([s]:gs) acc = collapseStatementGroups gs (s:acc)
collapseStatementGroups ((s:ss):gs) acc =
         let acc' = List.foldl (\a s' -> List.map (changeStatementReference (toStatementName s') (toStatementName s)) a) acc ss in
         let gs' = List.foldl (\g s' -> List.map (\gg -> (changeStatementReference (toStatementName s') (toStatementName s) $ List.head gg) :
                                        (List.tail gg)) g) gs ss in
         collapseStatementGroups gs' (s:acc')

-- [[a1],[a1,a2,a3]]
-- foldl :: (a -> b -> a) -> a -> [b] -> a
foldEqualCode :: TheoryC a k c c' s t => t -> IO [s]
foldEqualCode th = do
                     let thl = toStatementList th
                     -- let thm = toStatementMap th
                     putStrLn $ "Grouping code, " ++ (show $ List.length thl) ++ " statements total..."
                     stsg <- Utils.groupListBy (Text.unpack . toFQName '.' . toStatementName) (eqStatementCode th) thl
                     putStrLn $ "Collapsing groups, " ++ (show $ List.length stsg) ++ " groups..."
                     return $ collapseStatementGroups stsg []

---------------------------------------------------------------------------------------------------

getUsedStatements :: TheoryC a k c c' s t => t -> s -> [s]
getUsedStatements th st = let stc = toStatementCode st in
                          let thm = toStatementMap th in
                          case stc of
                            Left _ -> []
                            Right cl -> let usm = catMaybes $ List.map (\u -> case u of
                                                                         Left _ -> Nothing
                                                                         Right e -> if isTheoriable e then
                                                                                       Map.lookup (toKey $ toKey e) thm
                                                                                    else Nothing) cl in
                                        List.nub usm

unfoldUses :: TheoryC a k c c' s t => [s] -> [s] -> t -> IO [s]
unfoldUses [] acct _ = return acct
unfoldUses (st:sts) acct th = do
                         putStrLn $ "Unfolding " ++ (Text.unpack $ toSuffix $ toStatementName st) ++ ", remaining " ++ (show $ List.length sts)
                         let thm = toStatementMap th
                         let b = List.notElem st acct 
                         -- let isExtractable u = (fst u /= SelfReference) && (fst u /= BoundVariable) && (fst u /= LocalConstructor)
                         let stc = toStatementCode st
                         case stc of
                           Left _ -> do
                                      putStrLn "Statement code is not parsed, skipping..."
                                      unfoldUses sts acct th
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
extractTerms th (tt:tts) = do
                              et <- do
                                  let fsts = List.filter (\s -> ((toSuffix $ toStatementName s) == tt)) $ toStatementList th
                                  r <- unfoldUsesList fsts th 
                                  return $ List.map (Common.partsort toStatementName partCompare) r                                  
                              r <- extractTerms th tts 
                              return $ et:r

shortenLibNames :: StatementC a k c c' s => [s] -> ([s], [(Text.Text, Text.Text)])
shortenLibNames sts = let preforig = List.nub $ mapped %~ (toPrefix '_' . toStatementName) $ sts in
                      let prefsh = Utils.shortenText preforig in
                      let chmap = zip preforig prefsh in
                      (sts, chmap) 


extractTermsCode :: TheoryC a k c c' s t => [Text.Text] -> t -> IO [[Text.Text]]
extractTermsCode ts th = do
                          extss' <- extractTerms th ts
                          putStrLn $ "Length of extracted terms: " ++ (show $ mapped.mapped %~ List.length $ extss')
                          let chlibmap = mapped.mapped %~ shortenLibNames $ extss'                         
                          return $ mapped.mapped %~ (\(exts, chmap) -> Text.concat $
                                                  List.map (\s -> (fromCodeWithPrefixMapA '_' (Map.fromList chmap) $ toStatementCode s) <>
                                                            "\n(* end " <> (toFQName '.' $ toStatementName s) <> " *)\n\n") exts) $ chlibmap

-----------------------------------------------------------------------------------------

checkTheoryConsistancy :: TheoryC a k c c' s t => t -> IO ()
checkTheoryConsistancy th = do
                              putStrLn "Checking for presense..."
                              let us = List.nub $ Prelude.concat $ List.map (getUsedStatements th) $ toStatementList th 
                              let thm = toStatementMap $ th 
                              forM_ us (\u -> if (Map.member (toKey $ toStatementName u) thm) then
                                    putStrLn $ "!Found: " ++ (show $ toFQName '.' $ toStatementName u)
                              else
                                    putStrLn $ "?Found: " ++ (show $ toFQName '.' $ toStatementName u))

checkTheoryAcyclessness :: TheoryC a k c c' s t => t -> IO ()
checkTheoryAcyclessness th = do
                           putStrLn "Checking for cycles..."
                           let thl = toStatementList th
                           let thm = toStatementMap th                             
                           forM_ thl (\s -> do
                                              putStrLn $ "Checking " ++ (show $ toFQName '.' $ toStatementName s)
                                              let us = getUsedStatements th s
                                              forM_ us (\u -> let ms' = Map.lookup (toKey $ toStatementName u) thm in
                                                              case ms' of
                                                                 Nothing -> putStrLn $ "   Dependence " ++ (show $ toFQName '.' $ toStatementName u) ++ " not found"
                                                                 Just s' -> let us' = getUsedStatements th s' in
                                                                            if (List.elem u us') then
                                                                               putStrLn $ "   Dependence " ++ (show $ toFQName '.' $ toStatementName u) ++ " form a cycle"
                                                                            else
                                                                               putStrLn $ "   Dependence " ++ (show $ toFQName '.' $ toStatementName u) ++ " is OK"))
                                                               
