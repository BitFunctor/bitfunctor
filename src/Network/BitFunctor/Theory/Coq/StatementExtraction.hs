{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.BitFunctor.Theory.Coq.StatementExtraction  where

import System.Process
import GHC.IO.Handle
import System.Exit
import System.IO
import qualified Text.ParserCombinators.Parsec as PS (parse)
import Data.Text as DT
import qualified Data.Text.Encoding as TE
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.List as List 
import qualified Data.ByteString as DBS (hGet)
import qualified Data.Map as Map
import qualified Data.String.Utils as SU
import Data.Foldable (foldlM)
import qualified Data.Time as Time (getCurrentTime)
import qualified System.Directory as SD (doesFileExist)
import Data.Aeson (toJSON)
import Data.Tuple (swap)

import qualified Network.BitFunctor.Common as Common
import Network.BitFunctor.Crypto.Hash.Types
import qualified Network.BitFunctor.Crypto.Hash as Hash
import Network.BitFunctor.Theory.Types
import qualified Network.BitFunctor.Theory.Coq.Constants as Constants
import qualified Network.BitFunctor.Identifiable as Ident
import qualified Network.BitFunctor.Theory.Coq.GlobFileParser as GP

-- libname, modname, name
type CoqTerm = (CoqKind, CoqStatementName)
type PreStatement = StatementA CoqTerm
type PreTheory = Map.Map CoqStatementName PreStatement

-- TODO: deal with multiple declarations without dots like
-- Variale a b: X.
-- Remove vernac comments
loadVernacCode :: String -> Int -> Maybe Int -> IO Code
loadVernacCode vfname pos1 (Just pos2) = do
                                     -- putStrLn $ "pos1 = " ++ (show pos1)
                                     -- putStrLn $ "pos2 = " ++ (show pos2)
                                     h <- openBinaryFile vfname ReadMode                                    
                                     hSeek h AbsoluteSeek (fromIntegral pos1)                                     
                                     let seekBackDot n = do
                                                        bs <- DBS.hGet h 1
                                                        -- seeking was at pos1 - (n-1)
                                                        if (TE.decodeUtf8 bs == DT.pack [Constants.coqStatementDelimiter]) then return (n-1)
                                                        else do
                                                           -- putStrLn $ show n
                                                           if (pos1 >= n) then do  
                                                              hSeek h AbsoluteSeek (fromIntegral $ pos1 - n)
                                                              seekBackDot (n+1)
                                                           else do
                                                              hSeek h AbsoluteSeek 0
                                                              return n
                                     n <- seekBackDot 1 
                                     let sz = pos2 - pos1 + 1 + n                                      
                                     if (sz > 0) then do 
                                        bs <- DBS.hGet h sz
                                        -- putStrLn $ "bs = " ++ (show bs)
                                        hClose h  
                                        return $ CoqText $ DT.pack $ List.dropWhileEnd (\c -> c/= Constants.coqStatementDelimiter)
                                               $ SU.strip $ DT.unpack $ TE.decodeUtf8 bs
                                     else
                                        return $ CoqText ""
loadVernacCode vfname pos1 Nothing = do
                                       h <- openBinaryFile vfname ReadMode
                                       lastpos <- hFileSize h
                                       loadVernacCode vfname pos1 (Just $ fromIntegral $ lastpos - 1)

fromGlobFileRawEntry :: GP.GlobFileName -> GP.GlobFileRawEntry -> Maybe (Int, PreStatement)
fromGlobFileRawEntry lib r = case (Constants.resourceKind $ GP.ekind r) of
                           Resource ->
                              let ln' = GP.elibname r in
                              let ln = if (Prelude.null ln') then lib else ln' in
                              let mn = GP.emodname r in                              
                              let sn = GP.ename r in
                              if (Prelude.null sn) then Nothing
                                  else                                       
                                    Just (GP.espos r, Statement (CoqStatementName (DT.pack ln) (DT.pack mn) (DT.pack sn)) (GP.ekind r) (CoqText "") (Ident.id $ Ident.ByBinary lib) [])
                           StopStatement -> Nothing
                           IgnorableRes -> Nothing

collectStatements0 :: [GP.GlobFileEntry] -> String -> GP.GlobFileName -> Maybe (Int, PreStatement) -> Maybe Int -> [PreStatement] -> IO [PreStatement]
collectStatements0 [] vfname _ (Just (pos1, cs)) mpos2  accs = do
                                                              cd <- loadVernacCode vfname pos1 mpos2  
                                                              return (cs {stcode = cd}:accs)
collectStatements0 [] _ _ Nothing _  accs = return accs
collectStatements0 (s:ss) vfname libname pcs@(Just (pos1, cs)) mpos2 accs = 
            case s of
                GP.GlobFileStatement r-> do
                                      -- putStrLn $ "Processing " ++ (show $ ekind r)
                                      case (GP.ekind r, stkind cs) of
                                        (Constructor, Inductive) ->                                              
                                             let pcons' = fromGlobFileRawEntry libname r in
                                             case pcons' of
                                               Nothing -> fail "Cannot collect constructor"
                                               Just (_, cons') -> 
                                                   let cons = cons' {stuses = [(stkind cs, stname cs)]} in
                                                   collectStatements0 ss vfname libname pcs Nothing (cons:accs)
                                        (Constructor, _) -> fail "Meeting constructor when collecting not-Inductive"
                                        _ -> do
                                               cd <- loadVernacCode vfname pos1 (Just $ fromMaybe ((GP.espos r) - 1)  mpos2)
                                               let accs' = (cs {stcode = cd}:accs)
                                               let newpcs = fromGlobFileRawEntry libname r
                                               collectStatements0 ss vfname libname newpcs Nothing accs'                     
                GP.GlobFileResource r -> do
                                        -- putStrLn $ "Processing " ++ (show $ ekind r)
                                        case (Constants.resourceKind $ GP.ekind r) of
                                         StopStatement -> 
                                                   let newpos2 = Just $ fromMaybe ((GP.espos r) - 1) mpos2 in
                                                   collectStatements0 ss vfname libname pcs newpos2 accs
                                         Resource ->
                                                   let r' = fromGlobFileRawEntry libname r in
                                                   case r' of
                                                    Nothing -> collectStatements0 ss vfname libname pcs Nothing accs 
                                                    Just (_, rs) ->
                                                      let newpcs = Just (pos1, cs {stuses = (stkind rs, stname rs):(stuses cs)}) in
                                                      collectStatements0 ss vfname libname newpcs Nothing accs
                                         IgnorableRes -> collectStatements0 ss vfname libname pcs Nothing accs
collectStatements0 (s:ss) vfname libname Nothing _ accs =
            case s of
                GP.GlobFileStatement r ->  let newpcs = fromGlobFileRawEntry libname r in
                                        collectStatements0 ss vfname libname newpcs Nothing accs                     
                GP.GlobFileResource r ->   collectStatements0 ss vfname libname Nothing Nothing accs

collectStatements sts vfname libname = collectStatements0 sts vfname libname Nothing Nothing []



fullPrintTerm l1 l2 m t = let l2' = if l1==l2 then "" else l2 in
   (Constants.coqExportLib l1) ++ (Constants.coqExportLib l2') ++ (Constants.coqImportMod m) ++ (Constants.coqPrintTerm t)
fullPrintType l1 l2 m t = let l2' = if l1==l2 then "" else l2 in
   (Constants.coqExportLib l1) ++ (Constants.coqExportLib l2') ++ (Constants.coqImportMod m) ++ (Constants.coqPrintType t)

-- :: statement kind -> statement name -> theory -> accumulated list of (statements, filenames) ->
-- (statement name, generated (or found file))

getPrintedStatement :: CoqTerm -> IO (Maybe (String, String))
getPrintedStatement (k, sts) =  do
                            let fqstname = DT.unpack $ fqStatementName sts
                            let mname = DT.unpack $ modname sts
                            let shname = DT.unpack $ sname sts
                            let lname = DT.unpack $ libname sts
                            date <- Time.getCurrentTime -- "2008-04-18 14:11:22.476894 UTC"
                            let sename = Constants.xtrFilePrefix ++ (toString $ Ident.id $ Ident.ByBinary (show date, sts)) 
                            let fwPname = Constants.xtrPrintFilePrefix ++ sename ++ Constants.vernacFileSuffix
                            let fwCname = Constants.xtrTypeFilePrefix ++ sename ++ Constants.vernacFileSuffix
                            putStrLn $ "Generating files for " ++ show k ++ " " ++ fqstname                                         
                            writeFile fwPname $ fullPrintTerm "" lname mname fqstname
                            writeFile fwCname $ fullPrintType "" lname mname fqstname
                            (ecP, s1P, _) <- readProcessWithExitCode Constants.coqExecutable [fwPname] []
                            (ecC, s1C, _) <- readProcessWithExitCode Constants.coqExecutable [fwCname] []
                            case (ecP, ecC, Constants.isAbnormallyPrinted k) of
                                (ExitFailure _ , _ , _) -> do
                                                             putStrLn ("Error in coqc: " ++ fwPname)
                                                             return Nothing
                                (ExitSuccess , ExitFailure _ , True) -> do
                                                             putStrLn ("Error in coqc: " ++ fwCname)
                                                             return Nothing
                                _ -> do
                                        let gfilename = DT.unpack $ libname sts
                                        let gmodname = DT.unpack $ modname sts
                                        -- Require Export "MainLib"                                                   
                                        let header = (Constants.coqExportLib gfilename) ++ (Constants.coqImportMod gmodname)
                                        -- get a term definition without name and with type at the end
                                        let prebody = SU.strip $ Prelude.head $ SU.split Constants.coqPrintCommentsDelimiter s1P
                                        -- get a short name and type as a list
                                        let pretypename = SU.split Constants.coqTypeDelimiter $ SU.strip
                                                          $ Prelude.head $ SU.split Constants.coqPrintCommentsDelimiter s1C
                                        -- retrieve short name
                                        let shortname = Prelude.head pretypename
                                        -- retrieve type name
                                        let typename = SU.strip $ SU.join Constants.coqTypeDelimiter
                                                       $ Prelude.tail pretypename
                                        -- strip the type from the prebody
                                        let typeStrippedBody = SU.strip
                                                               $ Common.removeEndFromString (Constants.coqTypeDelimiter ++ typename)
                                                               $ SU.join Constants.coqPrintEqSign $ Prelude.tail
                                                               $ SU.split Constants.coqPrintEqSign prebody
                                        -- construct the body
                                        let body =  if Constants.isAbnormallyPrinted k then
                                                       Constants.coqDefineTerm shortname typename typeStrippedBody
                                                    else
                                                       prebody ++ [Constants.coqStatementDelimiter]
                                        -- construct the file contents
                                        return $ Just (header, body)


generateUnresolvedFile :: CoqTerm -> PreTheory -> [(CoqStatementName, String)] -> IO (Maybe (CoqStatementName, String))
generateUnresolvedFile ct@(k, sts) thm filem =
                                    if (Map.member sts thm) || (Constants.resourceKind k /= Resource) then return Nothing
                                    else do
                                        mhb <- getPrintedStatement ct
                                        case mhb of
                                          Nothing -> do
                                                      putStrLn "Cannot generate term body"
                                                      return Nothing
                                          Just (header, body) -> do
                                                  let mname = DT.unpack $ modname sts
                                                  let fqstname = DT.unpack $ fqStatementName sts  
                                                  let newst = header ++ body                              
                                                  let idChunk = Ident.id $ Ident.ByBinary (libname sts, mname, body)
                                                  let newfn = Constants.generatedFilePrefix ++ (toString idChunk)
                                                  let idFile = Ident.id $ Ident.ByBinary newfn
                                                  let mfile = Map.lookup newfn $ Map.fromList $ List.map swap filem
                                                  bFileExists <- SD.doesFileExist $ newfn ++ Constants.vernacFileSuffix
                                                  if (bFileExists) then do
                                                             putStrLn ("- file already generated")
                                                             return $ Just (sts, newfn)
                                                  else case mfile of
                                                     Just _ -> do
                                                                     fail "- file generated but doesn't exist" 
                                                     Nothing -> do
                                                                 let thm' = Map.fromList $ List.map (\(_,ps) -> (stsource ps, stname ps))
                                                                                         $ Map.toList thm    
                                                                 let mfile2 = Map.lookup idFile thm'
                                                                 case mfile2 of
                                                                   Just name -> do
                                                                                  putStrLn "- found in the Theory, but file doesn't exist?"
                                                                                  return $ Just (sts, newfn)
                                                                   Nothing -> do
                                                                                let frname = newfn ++ Constants.vernacFileSuffix
                                                                                putStrLn $ "Writing chunk for " ++ fqstname
                                                                                            ++ ":\n" ++ newst
                                                                                writeFile frname newst
                                                                                return $ Just (sts, newfn)
                                

--(a -> b -> a) -> a -> [b] -> a
--(b -> a -> m b) -> b -> t a -> m b
-- all new statements -> theory -> list of (sts, filenames) 
generateUnresolvedFiles :: [CoqTerm] -> PreTheory -> IO [(CoqStatementName, String)]
generateUnresolvedFiles cts thm = do                                     
                                    mfiles <- foldlM (\fm u -> do
                                                               mts <- generateUnresolvedFile u thm fm
                                                               case mts of
                                                                 -- already in theory or impossible to generate 
                                                                 Nothing -> return fm
                                                                 Just x -> return $ (x:fm)  
                                                         ) [] cts
                                    -- nub could be omitted if all correct
                                    return $ List.nub mfiles

generateUnresolvedFilesForDependencies sts thm = generateUnresolvedFiles (List.nub $ Prelude.concat $ List.map stuses sts) thm
                                        
-- after generation new files, original lib name should be changed to the filenames generated 
changeTermLib :: CoqTerm -> Map.Map CoqStatementName String -> CoqTerm
changeTermLib (k,t) m = let filen = Map.lookup t m in                        
                        case filen of
                          Nothing -> (k,t)
                          Just fn -> (k, CoqStatementName (DT.pack fn) "" (sname t))

-- removes dublicates
-- removes dublicates in "uses"
-- removes self-referencing from "uses"
-- removes Variables from "uses" as they are local and bound (global are referenced as Axioms)

-- rename!
removeSomeStatements :: [PreStatement] -> [PreStatement]
removeSomeStatements sts = List.filter (\s -> stkind s /= Constructor) sts

adjustStatements :: [PreStatement] -> [PreStatement]
adjustStatements = List.nubBy eqStatement .
                     List.map (\s -> s {stuses = List.filter (\u -> u /= (stkind s, stname s) && fst u /= Variable) $ List.nub $ stuses s})

-- think of kind
eqStatement :: PreStatement -> PreStatement -> Bool
eqStatement s1 s2 = (stname s1 == stname s2) &&
                    (stkind s1 == stkind s2)

-- NB!: if used term is declared inside internal module
-- need to look upper
-- that is bug in Coq
rereferInductives:: [PreStatement] -> [PreStatement]
rereferInductives sts = let m = Map.fromList $ List.map (\s -> (stname s, s)) sts in
                        let look st =
                                   let mref = Map.lookup st m in
                                   case mref of
                                      Nothing -> let mod = DT.unpack $ modname st in 
                                                 if (Prelude.null mod) then Nothing
                                                 else 
                                                   let mod' = List.dropWhileEnd (\c -> c /= Constants.coqModuleDelimiter) $ mod in
                                                   let mod'' = List.dropWhileEnd (\c -> c == Constants.coqModuleDelimiter) $ mod' in
                                                   let st' = st {modname = DT.pack mod''} in
                                                   look st'                                                   
                                      Just ref -> Just ref in
                        List.map (\s -> s {stuses = List.map (\u -> let mref = look (snd u) in
                                                                    let u' = case mref of
                                                                               Nothing -> u
                                                                               Just ref -> (fst u, stname ref) in
                                                                    if (fst u' == Constructor) then
                                                                       let mcons = Map.lookup (snd u) m in
                                                                       case mcons of
                                                                         Nothing -> u'
                                                                         Just constr -> Common.headWithDefault u' $ stuses constr
                                                                    else u') $ stuses s}) sts

adjustRecursive :: [PreStatement] -> [PreStatement]
adjustRecursive sts = let sts' = adjustStatements $ rereferInductives sts in
                      if (sts'==sts) then sts
                      else adjustRecursive sts'

-- obsolete
isExtractedbyPrint :: PreStatement -> Bool
isExtractedbyPrint s = DT.isPrefixOf (DT.pack Constants.generatedFilePrefix) (libname $ stname s)

extractStatements0 :: [String] -> [String] -> [PreStatement] -> IO [PreStatement]
extractStatements0 [] accf acc = return acc
extractStatements0 (fn:fs) accf acc = do                            
                            if (List.elem fn accf) then do
                                                      putStrLn $ "File already processed: " ++ fn
                                                      extractStatements0 fs accf acc
                            else do
                               let vFile = fn ++ Constants.vernacFileSuffix
                               let gFile = fn ++ Constants.globFileSuffix
                               (ec, s1, _) <- readProcessWithExitCode Constants.coqExecutable [vFile] []
                               case ec of
                                 ExitFailure _ -> do
                                                   putStrLn ("Error in coqc: " ++ vFile)
                                                   extractStatements0 fs (fn:accf) acc
                                 ExitSuccess ->	 do
                                                   putStrLn ("coqc output:\n" ++ s1)
                                                   globfile  <- readFile gFile                                                  
                                                   case (PS.parse GP.globfileData "" globfile) of
                                                       Left err -> do
                                                                     putStrLn "Parse error: " >> print gFile >> print err
                                                                     extractStatements0 fs (fn:accf) acc
                                                       Right (dig, lib, ent)  -> do
                                                                   sts' <- collectStatements ent vFile lib
                                                                   let sts = adjustRecursive sts'
                                                                   let newacc = adjustRecursive $ sts ++ acc
                                                                   let thm = Map.fromList $ List.map (\s -> (stname s, s)) newacc
                                                                   newfiles <- generateUnresolvedFilesForDependencies sts thm
                                                                   let newnames = Map.fromList newfiles
                                                                   let chacc = List.map (\s -> s{stuses = List.map (\u -> changeTermLib u newnames) $ stuses s}) newacc
                                                                   let newacc' = adjustRecursive chacc 
                                                                   let newfiles' = List.nub $ (List.map snd newfiles) ++ fs
                                                                   putStrLn $ "File " ++ fn ++ " has been processed, remaining " ++ (show $ List.length newfiles')
                                                                   extractStatements0 newfiles' (fn:accf) newacc'
                                      

                           

extractSymbols :: String -> IO [CoqTerm]
extractSymbols fn = do
                     let vFile = fn ++ Constants.vernacFileSuffix
                     let gFile = fn ++ Constants.globFileSuffix
                     (ec, s1, _) <- readProcessWithExitCode Constants.coqExecutable [vFile] []
                     case ec of
                        ExitFailure _ -> do
                                          putStrLn ("Error in coqc: " ++ vFile)
                                          return []
                        ExitSuccess ->	 do
                                                   -- putStrLn ("coqc output:\n" ++ s1)
                                           globfile  <- readFile gFile                                                  
                                           case (PS.parse GP.globfileData "" globfile) of
                                               Left err -> do
                                                            putStrLn "Parse error: " >> print gFile >> print err
                                                            return []
                                               Right (dig, lib, ent)  -> do
                                                            sts' <- collectStatements ent vFile lib
                                                            let sts = removeSomeStatements $ adjustRecursive sts'
                                                            return (List.map (\s -> (stkind s, stname s)) sts)

-- IO (Maybe (CoqStatementName, String))
printCoqSymbols :: [CoqTerm] -> IO [String]
printCoqSymbols [] = return []
printCoqSymbols (ct:cts) = do
                             mgct <- generateUnresolvedFile ct Map.empty []
                             fns <- printCoqSymbols cts
                             case mgct of
                               Nothing -> return fns
                               Just (_, fn) -> return (fn:fns)  

extractStatements :: [String] -> IO [PreStatement]
extractStatements fns = do
                          newfs' <- mapM (\fn -> extractSymbols fn >>= printCoqSymbols) fns
                          let newfs = Prelude.concat newfs'
                          extractStatements0 newfs [] [] 

unfoldUses :: [PreStatement] -> PreTheory -> [[PreStatement]]
unfoldUses [] _ = []
unfoldUses (t:ts) thm = ut:(unfoldUses ts thm) where
                        ut = let usm = catMaybes $ List.map (\u -> Map.lookup (snd u) thm) (stuses t) in
                             List.nub $ (t:(Prelude.concat $ unfoldUses usm thm))


ordStatement :: PreStatement -> PreStatement -> Common.PartOrdering
ordStatement s1 s2 = if (List.elem (stname s1) (List.map snd $ stuses s2)) then Common.PLT else
                     if (List.elem (stname s2) (List.map snd $ stuses s1)) then Common.PGT else
                     if (stname s1 == stname s2) then Common.PEQ else
                     Common.PNC

extractTerms :: [String] -> [PreStatement] -> [[[PreStatement]]]
extractTerms [] _ = []
extractTerms (t:ts) sts = et:(extractTerms ts sts) where
                          et = let fsts = List.filter (\s -> (sname $ stname s) == (DT.pack t)) sts in
                               let m = Map.fromList $ List.map (\s -> (stname s, s)) sts in
                               List.map (Common.partsort stname ordStatement) $ unfoldUses fsts m

extractTermsCode :: [String] -> [PreStatement] -> [[Text]]
extractTermsCode ts sts = let extss = extractTerms ts sts in
                          List.map (List.map (\ext -> DT.concat $ List.map (\s -> DT.append (fromCode $ stcode s)
                                                      $ DT.append "\n(* end " $ DT.append (sname $ stname s) " *)\n\n") ext)) extss
