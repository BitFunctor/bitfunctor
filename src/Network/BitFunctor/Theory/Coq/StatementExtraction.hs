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
import qualified Data.Text.IO as TextIO (writeFile)
import qualified Data.Text.Encoding as TE
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.List as List 
import qualified Data.ByteString as DBS
import qualified Data.Map as Map
import qualified Data.String.Utils as SU
import Data.Foldable (foldlM)
import qualified Data.Time as Time (getCurrentTime)
import qualified System.Directory as SD (doesFileExist, removeFile)
import Data.Tuple (swap)
import Control.Monad (forM_, mapM)
import qualified Data.ByteString.Char8 as DBS8

import qualified Network.BitFunctor.Common as Common
import Network.BitFunctor.Crypto.Hash.Types
import qualified Network.BitFunctor.Crypto.Hash as Hash
import Network.BitFunctor.Theory.Types
import qualified Network.BitFunctor.Theory.Coq.Constants as Constants
import qualified Network.BitFunctor.Identifiable as Ident
import qualified Network.BitFunctor.Theory.Coq.GlobFileParser as GP

type CoqTermEntry = (CoqTerm, [GP.GlobFilePosition])
type PreStatementWithPositions = StatementA CoqTermEntry
type PreTheoryWithPositions = Map.Map CoqStatementName PreStatementWithPositions

{--
type CoqTerm = (CoqKind, CoqStatementName)
type CoqTermEntry = (CoqTerm, [GP.GlobFilePosition])
type PreStatementWithPositions = StatementA CoqTermEntry
type PreTheory a = Map.Map CoqStatementName (StatementA a)
type PreTheoryWithPositions = Map.Map CoqStatementName PreStatementWithPositions
type PreStatementWithList = StatementA CoqTerm
type PreTheoryWithList = PreTheory CoqTerm
--}

(<>) :: DT.Text -> DT.Text -> DT.Text
(<>) t1 t2 = DT.append t1 t2

-- if the final term is stripped we could lose the shift number for dependencies
-- so it will not be stipped, always having newline as the first symbol - and that is OK only for Printed term
-- if to take from the user code some extra spaces could be added to the final Code
{--
loadVernacCode :: FilePath -> Int -> Maybe Int -> IO (Int, Code a)
loadVernacCode vfname pos1 (Just pos2) = do                                     
                                     h <- openBinaryFile vfname ReadMode                                    
                                     hSeek h AbsoluteSeek (fromIntegral pos1)                                     
                                     let seekBackDot n = do
                                                        bs <- DBS.hGet h 1
                                                        -- seeking was at pos1 - (n-1)
                                                        if (TE.decodeUtf8 bs == DT.singleton Constants.coqStatementDelimiter) then return (n-1)
                                                        else do                                                           
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
                                        hClose h  
                                        return $ (pos1 - n + 1, CoqText $ DT.dropWhileEnd (\c -> c /= Constants.coqStatementDelimiter)
                                               -- $ SU.strip
                                               $ TE.decodeUtf8 bs)
                                     else
                                        return $ (0, CoqText "")
loadVernacCode vfname pos1 Nothing = do
                                       h <- openBinaryFile vfname ReadMode
                                       lastpos <- hFileSize h
                                       loadVernacCode vfname pos1 (Just $ fromIntegral $ lastpos - 1)
--}

loadVernacCode :: FilePath -> Int -> Maybe Int -> IO (Int, Code a)
loadVernacCode vfname pos1 (Just pos2) = do                                     
                                     h <- openBinaryFile vfname ReadMode
                                     fcont <- DBS8.hGetContents h                                                                                                           
                                     let seekBackDot n = let c = DBS8.index fcont (pos1 - n) in
                                                         if (c == Constants.coqStatementDelimiter) then (n-1)
                                                         else                                                           
                                                           if (pos1 > n) then                                                                 
                                                              seekBackDot (n+1)
                                                           else n
                                     let n = seekBackDot 1 
                                     let sz = pos2 - (pos1 - n) +1                                      
                                     if (sz > 0) then do 
                                        let bs = DBS8.take sz $ DBS8.drop (pos1 - n) fcont
                                        hClose h
                                        return $ (pos1 - n, CoqText $ DT.dropWhileEnd (\c -> c /= Constants.coqStatementDelimiter)
                                               -- $ SU.strip
                                               $ TE.decodeUtf8 bs)
                                     else
                                        return $ (0, CoqText "")
loadVernacCode vfname pos1 Nothing = do
                                       h <- openBinaryFile vfname ReadMode
                                       lastpos <- hFileSize h
                                       loadVernacCode vfname pos1 (Just $ fromIntegral $ lastpos - 1)




fromGlobFileRawEntry :: GP.GlobFileName -> GP.GlobFileRawEntry -> Maybe (GP.GlobFilePosition, PreStatementWithPositions)
fromGlobFileRawEntry lib r = case (Constants.resourceKind $ GP.ekind r) of
                           Resource ->
                              let ln' = GP.elibname r in
                              let ln = if (Prelude.null ln') then lib else ln' in
                              let mn = GP.emodname r in                              
                              let sn = GP.ename r in
                              let csn = CoqStatementName (DT.pack ln) (DT.pack mn) (DT.pack sn) in 
                              if (Prelude.null sn) then Nothing
                                  else                                       
                                    Just (GP.epos r, Statement csn (GP.ekind r) (CoqText "") (Ident.id $ Ident.ByBinary lib) [((SelfReference, csn),[GP.epos r])])
                           StopStatement -> Nothing
                           IgnorableRes -> Nothing

adjustGlobFilePosition :: Int -> GP.GlobFilePosition -> GP.GlobFilePosition
adjustGlobFilePosition n (GP.GlobFilePosition x y) = GP.GlobFilePosition (x+n) (y+n)

-- TODO: do not collect Code when firstly process user file
-- the code collection should be done only for Printed terms
collectStatements0 :: [GP.GlobFileEntry] -> FilePath -> GP.GlobFileName -> Maybe (GP.GlobFilePosition, PreStatementWithPositions) -> Maybe Int -> [PreStatementWithPositions] -> IO [PreStatementWithPositions]
collectStatements0 [] vfname _ (Just (GP.GlobFilePosition pos1 _, cs)) mpos2  accs = do
                                                              (n, cd) <- loadVernacCode vfname pos1 mpos2  
                                                              return $ cs {stcode = cd,
                                                                          stuses = List.map (\u -> (fst u, List.map (adjustGlobFilePosition (-n)) $ snd u)) (stuses cs)}:accs
collectStatements0 [] _ _ Nothing _  accs = return accs
collectStatements0 (s:ss) vfname libname pcs@(Just jpcs@(GP.GlobFilePosition pos1 _, cs)) mpos2 accs = 
            case s of
                GP.GlobFileStatement r-> do                                      
                                      case (GP.ekind r, stkind cs) of
                                        (Constructor, Inductive) ->                                              
                                             let pcons' = fromGlobFileRawEntry libname r in
                                             case pcons' of
                                               Nothing -> fail "Cannot collect constructor"
                                               Just (pos, cons') -> 
                                                   let cons = cons' in 
                                                   collectStatements0 ss vfname libname (Just (fst jpcs, (snd jpcs) {stuses = Map.toList $
                                                                                                                     Map.insertWith (++) (LocalConstructor, stname cons') [pos] $
                                                                                                                     Map.fromList (stuses $ snd jpcs)})) Nothing (cons:accs)
                                        (Constructor, _) -> fail "Meeting constructor when collecting not-Inductive"
                                        _ -> do
                                               (n, cd) <- loadVernacCode vfname pos1 (Just $ fromMaybe ((GP.espos $ GP.epos r) - 1)  mpos2)
                                               let accs' = cs {stcode = cd,
                                                                          stuses = List.map (\u -> (fst u, List.map (adjustGlobFilePosition (-n)) $ snd u)) (stuses cs)}:accs
                                               let newpcs = fromGlobFileRawEntry libname r
                                               collectStatements0 ss vfname libname newpcs Nothing accs'                     
                GP.GlobFileResource r -> do                                        
                                        case (Constants.resourceKind $ GP.ekind r) of
                                         StopStatement -> 
                                                   let newpos2 = Just $ fromMaybe ((GP.espos $ GP.epos r) - 1) mpos2 in
                                                   collectStatements0 ss vfname libname pcs newpos2 accs
                                         Resource ->
                                                   let r' = fromGlobFileRawEntry libname r in
                                                   case r' of
                                                    Nothing -> collectStatements0 ss vfname libname pcs Nothing accs 
                                                    Just (pos, rs) ->
                                                      let newpcs = Just (GP.GlobFilePosition pos1 pos1, cs {stuses = Map.toList $
                                                                                                                     Map.insertWith (++) (stkind rs, stname rs) [pos] $
                                                                                                                     Map.fromList (stuses cs)}) in
                                                      collectStatements0 ss vfname libname newpcs Nothing accs
                                         IgnorableRes -> collectStatements0 ss vfname libname pcs Nothing accs
collectStatements0 (s:ss) vfname libname Nothing _ accs =
            case s of
                GP.GlobFileStatement r ->  let newpcs = fromGlobFileRawEntry libname r in
                                        collectStatements0 ss vfname libname newpcs Nothing accs                     
                GP.GlobFileResource r ->   collectStatements0 ss vfname libname Nothing Nothing accs

collectStatements sts vfname libname = collectStatements0 sts vfname libname Nothing Nothing []

removeFiles :: [FilePath] -> IO ()
removeFiles [] = return ()
removeFiles (f:fs) = do
                    bFileExists <- SD.doesFileExist f
                    if bFileExists then
                       SD.removeFile f
                    else
                       removeFiles fs
                    removeFiles fs

clearCoqGeneratedFiles :: [FilePath] -> IO ()
clearCoqGeneratedFiles [] = return ()
clearCoqGeneratedFiles (f:fs) = removeFiles [f++Constants.vernacFileSuffix, f++Constants.vernacBinaryFileSuffix, f++Constants.globFileSuffix] >> clearCoqGeneratedFiles fs

fullPrintTerm l1 l2 m t = let l2' = if l1 == l2 then "" else l2 in
   (Constants.coqExportLib l1) <> (Constants.coqExportLib l2') <> (Constants.coqImportMod m) <> (Constants.coqPrintTerm t)
fullPrintType l1 l2 m t = let l2' = if l1 == l2 then "" else l2 in
   (Constants.coqExportLib l1) <> (Constants.coqExportLib l2') <> (Constants.coqImportMod m) <> (Constants.coqPrintType t)

getPrintedStatement :: CoqTerm -> IO (Maybe (DT.Text, DT.Text))
getPrintedStatement (k, sts) =  do
                            let fqstname = fqStatementName sts
                            let mname = modname sts
                            let shname = sname sts
                            let lname = libname sts
                            date <- Time.getCurrentTime -- "2008-04-18 14:11:22.476894 UTC"
                            let sename = Constants.xtrFilePrefix ++ (toString $ Ident.id $ Ident.ByBinary (show date, sts))
                            let fwPname' = Constants.xtrPrintFilePrefix ++ sename
                            let fwCname' = Constants.xtrTypeFilePrefix ++ sename
                            let fwPname = fwPname' ++ Constants.vernacFileSuffix
                            let fwCname = fwCname' ++ Constants.vernacFileSuffix
                            putStrLn $ "Generating files for " ++ show k ++ " " ++ (DT.unpack fqstname)
                            TextIO.writeFile fwPname $ fullPrintTerm "" lname mname fqstname
                            TextIO.writeFile fwCname $ fullPrintType "" lname mname fqstname
                            (ecP, s1P', _) <- readProcessWithExitCode Constants.coqExecutable [fwPname] []
                            (ecC, s1C', _) <- readProcessWithExitCode Constants.coqExecutable [fwCname] []
                            case (ecP, ecC, Constants.isAbnormallyPrinted k) of
                                (ExitFailure _ , _ , _) -> do
                                                             putStrLn ("Error in coqc: " ++ fwPname)
                                                             clearCoqGeneratedFiles [fwPname', fwCname'] 
                                                             return Nothing
                                (ExitSuccess , ExitFailure _ , True) -> do
                                                             putStrLn ("Error in coqc: " ++ fwCname)
                                                             clearCoqGeneratedFiles [fwPname', fwCname'] 
                                                             return Nothing
                                _ -> do
                                        let s1P = DT.pack s1P'
                                        let s1C = DT.pack s1C'
                                        let gfilename = libname sts
                                        let gmodname = modname sts
                                        -- Require Export "MainLib"                                                   
                                        let header = (Constants.coqExportLib gfilename) <> (Constants.coqImportMod gmodname)
                                        -- get a term definition without name and with type at the end
                                        let prebody = DT.strip $ Prelude.head $ DT.splitOn Constants.coqPrintCommentsDelimiter s1P
                                        -- get a short name and type as a list
                                        let firstword = Prelude.head $ DT.splitOn Constants.coqSpace prebody
                                        let pretypename = DT.splitOn Constants.coqTypeDelimiter $ DT.strip
                                                          $ Prelude.head $ DT.splitOn Constants.coqPrintCommentsDelimiter s1C
                                        -- retrieve short name
                                        let shortname = Prelude.head pretypename
                                        -- retrieve type name
                                        let typename = DT.strip $ DT.intercalate Constants.coqTypeDelimiter
                                                       $ Prelude.tail pretypename
                                        -- strip the type from the prebody
                                        let typeStrippedBody = DT.strip
                                                               $ Common.removeEndFromText (Constants.coqTypeDelimiter <> typename)
                                                               $ DT.intercalate Constants.coqPrintEqSign $ Prelude.tail
                                                               $ DT.splitOn Constants.coqPrintEqSign prebody
                                        -- construct the body
                                        -- putStrLn $ "First word: " ++ firstword
                                        -- the hack with firstword is needed in particular for classes, extracted as inductives
                                        -- so when firstword==shortname it cannot be compiled, so must be processed alternatively
                                        -- but otherwise could be used as it is
                                        let body =  if k == Axiom || k == Variable then
                                                       Constants.coqDefineTerm shortname typename ""
                                                    else 
                                                       if (Constants.isAbnormallyPrinted k) && (DT.strip firstword == DT.strip shortname) then 
                                                          Constants.coqDefineTerm shortname typename typeStrippedBody
                                                       else
                                                          prebody <> (DT.singleton Constants.coqStatementDelimiter)
                                        -- construct the file contents
                                        clearCoqGeneratedFiles [fwPname', fwCname'] 
                                        return $ Just (header, body)


generateUnresolvedFile :: CoqTerm -> PreTheory a -> [(CoqStatementName, FilePath)] -> IO (Maybe (CoqStatementName, FilePath))
generateUnresolvedFile ct@(k, sts) thm filem =
                             if (Map.member sts thm) || (Constants.resourceKind k /= Resource) then return Nothing
                             else 
                                   let mf = Map.lookup sts $ Map.fromList filem in
                                   case mf of
                                    Just f -> return Nothing
                                    Nothing -> do
                                        mhb <- getPrintedStatement ct
                                        case mhb of
                                          Nothing -> do
                                                      putStrLn "Cannot generate term body"
                                                      return Nothing
                                          Just (header, body) -> do
                                                  let mname = modname sts
                                                  let fqstname = fqStatementName sts  
                                                  let newst = header <> body                              
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
                                                                                putStrLn $ DT.unpack $ "Writing chunk for " <> fqstname <> ":\n" <> newst
                                                                                TextIO.writeFile frname newst
                                                                                return $ Just (sts, newfn)
                                

--(a -> b -> a) -> a -> [b] -> a
--(b -> a -> m b) -> b -> t a -> m b
-- all new statements -> theory -> list of (sts, filenames) 
generateUnresolvedFiles :: [CoqTerm] -> PreTheory a -> [(CoqStatementName, FilePath)] -> IO [(CoqStatementName, FilePath)]
generateUnresolvedFiles cts thm tsf = do                                     
                                    mfiles <- foldlM (\fm u -> do
                                                               mts <- generateUnresolvedFile u thm (fm ++ tsf)
                                                               case mts of
                                                                 -- already in theory or impossible to generate 
                                                                 Nothing -> return fm
                                                                 Just x -> return $ (x:fm)  
                                                         ) [] cts
                                    -- nub could be omitted if all correct
                                    return mfiles -- $ List.nub mfiles 

generateUnresolvedFilesForDependencies sts thm tsf = generateUnresolvedFiles (List.nub $ Prelude.concat $ List.map (List.map fst . stuses) sts) thm tsf
                                        
-- after generation new files, original lib name should be changed to the filenames generated 
changeTermLib :: CoqTerm -> Map.Map CoqStatementName String -> CoqTerm
changeTermLib (k,t) m = let filen = Map.lookup t m in                        
                        case filen of
                          Nothing -> (k,t)
                          Just fn -> (k, CoqStatementName (DT.pack fn) "" (sname t))

-- rename this, removes only constructors for the first stage of user file processing!
removeSomeStatements :: [StatementA a] -> [StatementA a]
removeSomeStatements sts = List.filter (\s -> stkind s /= Constructor) sts

adjustStatements :: [PreStatementWithPositions] -> [PreStatementWithPositions]
adjustStatements =  -- List.nubBy eqStatement .
                    List.map (\s -> s {stuses = List.map (\u -> let pos = List.nub $ snd u in
                                                                if (fst u == (stkind s, stname s)) then
                                                                   ((SelfReference, stname s), pos)
                                                                else
                                                                   if (fst . fst) u == Variable then
                                                                      ((BoundVariable, snd $ fst u), pos)
                                                                   else (fst u, pos)) $ stuses s})

-- think of kind
eqStatement :: StatementA a -> StatementA a -> Bool
eqStatement s1 s2 = (stname s1 == stname s2) &&
                    (stkind s1 == stkind s2)

-- NB!: if used term is declared inside internal module
-- need to look upper
-- that is bug in Coq

{-- seems to be obsolete
rereferInductives:: [PreStatementWithPositions] -> [PreStatementWithPositions]
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
                        List.map (\s -> s {stuses = List.map (\u -> let mref = look (snd $ fst u) in
                                                                    let u' = case mref of
                                                                               Nothing -> u
                                                                               Just ref -> ((fst $ fst u, stname ref), snd u)
                                                                    in u'
                                                              ) $ stuses s}) sts 


adjustRecursive :: [PreStatementWithPositions] -> [PreStatementWithPositions]
adjustRecursive sts = let sts' = adjustStatements $ rereferInductives sts in
                      if (sts' == sts) then sts
                      else adjustRecursive sts'
--}

-- do not know whether it is efficient to do like this
-- but the position is given in bytes and chars may occupy more bytes
-- so we use encode/decode technique to count the shift in 8bit bytes
takeFromCode :: Code CoqTermEntry -> GP.GlobFilePosition -> DT.Text
takeFromCode (CoqText t) (GP.GlobFilePosition s e) = TE.decodeUtf8
                                                     $ DBS.take (fromIntegral (e-s+1))
                                                     $ DBS.drop (fromIntegral s)
                                                     $ TE.encodeUtf8 t
takeFromCode (CoqCodeParts l) _ = ""


checkDependencies :: [PreStatementWithPositions] -> IO [PreStatementWithPositions]
checkDependencies sts = do
                          forM_ sts (\s -> do
                             putStrLn $ "\nChecking for " ++ (DT.unpack (fqStatementName $ stname s)) ++ " --->"
                             forM_ (stuses s) (\u -> do
                                                      putStrLn $ "  " ++ (DT.unpack (fqStatementName $ snd $ fst u)) ++ " ->"
                                                      forM_ (snd u) (\p -> do                                                                           
                                                                            let codestr = DT.unpack $ takeFromCode (stcode s) p
                                                                            let sn = DT.unpack $ sname $ snd $ fst u
                                                                            if (codestr == sn) then
                                                                                putStrLn $ "OK1:    '" ++ sn ++ "' " ++ "'" ++ codestr ++ "'"
                                                                            else
                                                                                if (SU.endswith sn codestr) then
                                                                                   putStrLn $ "OK2:    '" ++ sn ++ "' " ++ "'" ++ codestr ++ "'"
                                                                                else 
                                                                                   putStrLn $ "Fail:    '" ++ sn ++ "' " ++ "'" ++ codestr ++ "' p:" ++ (show p))))
                          return sts

fromCode :: Code CoqTerm -> Text
fromCode (CoqText t)  = t
fromCode (CoqCodeParts l) = List.foldl (\acct e -> case e of
                                                CoqCodeText t -> DT.append acct t
                                                CoqCodeEntry ct -> DT.append acct $ DT.append (if (fst ct /= BoundVariable) then ((libname $ snd ct) <> "_")  else "") $ sname $ snd ct) "" l


-- foldl :: (a -> b -> a) -> a -> [b] -> a
transformCode :: [CoqTermEntry] -> Code CoqTermEntry -> Code CoqTerm
transformCode _ ct@(CoqText "") = CoqCodeParts []
transformCode uslist ct@(CoqText t) = let uslist' = Prelude.concat $ List.map (\u -> List.map (\p -> (fst u, p)) $ snd u) uslist in
                                      let ussorted = List.sortBy (\u1 u2 -> compare (GP.espos $ snd u1) (GP.espos $ snd u2)) uslist' in                                   
                                      let (codeparts', p) = List.foldl (\(l,p) u -> if (GP.espos $ snd u) > p then (l ++ [CoqCodeText $ takeFromCode ct $ GP.GlobFilePosition p ((GP.espos $ snd u) - 1)] ++
                                                                                                            [CoqCodeEntry $ fst u],
                                                                                                       (GP.eepos $ snd u) + 1)
                                                                                                 else (l ++ [CoqCodeEntry $ fst u], (GP.eepos $ snd u) + 1)) ([],0) ussorted in
                                   CoqCodeParts $ codeparts' ++ [CoqCodeText $ takeFromCode ct $ GP.GlobFilePosition p ((DBS.length $ TE.encodeUtf8 t) - 1)]
                                   

-- suggest to be never called
transformCode _ (CoqCodeParts pts) = CoqCodeParts (List.map (\pt -> case pt of
                                                                     CoqCodeText t -> CoqCodeText t
                                                                     CoqCodeEntry p -> CoqCodeEntry (fst p)) pts)


transformStatement :: PreStatementWithPositions -> PreStatementWithList
transformStatement (Statement stn stk stc sts stu) = Statement stn stk (transformCode stu stc) sts (List.map fst stu) 

-- files to process -> processed files -> map of extracted sts to files -> accumulated list of extracted sts -> final list of extracted sts
extractStatements0 :: [String] -> [String] -> [(CoqStatementName, String)] -> [PreStatementWithPositions] -> IO [PreStatementWithList]
extractStatements0 [] accf acct accs = return $ List.map transformStatement accs -- checkDependencies acc
extractStatements0 (fn:fs) accf acct accs = do                            
                            if (List.elem fn accf) then do
                                                      putStrLn $ "File already processed: " ++ fn
                                                      extractStatements0 fs accf acct accs
                            else do
                               let vFile = fn ++ Constants.vernacFileSuffix
                               let gFile = fn ++ Constants.globFileSuffix
                               (ec, s1, _) <- readProcessWithExitCode Constants.coqExecutable [vFile] []
                               case ec of
                                 ExitFailure _ -> do
                                                   putStrLn ("Error in coqc: " ++ vFile)
                                                   extractStatements0 fs (fn:accf) acct accs
                                 ExitSuccess ->	 do
                                                   -- putStrLn ("coqc output:\n" ++ s1)
                                                   globfile  <- readFile gFile                                                  
                                                   case (PS.parse GP.globfileData "" globfile) of
                                                       Left err -> do
                                                                     putStrLn "Parse error: " >> print gFile >> print err
                                                                     extractStatements0 fs (fn:accf) acct accs
                                                       Right (dig, lib, ent)  -> do
                                                                   sts' <- collectStatements ent vFile lib
                                                                   let sts = adjustStatements sts'
                                                                   let newacc = sts ++ accs
                                                                   let thm = Map.fromList $ List.map (\s -> (stname s, s)) newacc                                                                   
                                                                   acct' <- generateUnresolvedFilesForDependencies sts thm acct
                                                                   let newacct = acct' ++ acct
                                                                   let newnames = Map.fromList newacct
                                                                   let chacc = List.map (\s ->
                                                                                  s{stuses = List.map (\u ->
                                                                                       (changeTermLib (fst u) newnames, snd u))
                                                                                         $ stuses s}) newacc
                                                                   let newacc' = chacc 
                                                                   let newfiles' = List.nub $ (List.map snd acct') ++ fs                                                                   
                                                                   putStrLn $ "File " ++ fn ++ " has been processed, remaining " ++ (show $ List.length newfiles')
                                                                   extractStatements0 newfiles' (fn:accf) newacct newacc'
                                      

                           

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
                                           globfile <- readFile gFile                                                  
                                           case (PS.parse GP.globfileData "" globfile) of
                                               Left err -> do
                                                            putStrLn "Parse error: " >> print gFile >> print err
                                                            return []
                                               Right (dig, lib, ent)  -> do
                                                            sts' <- collectStatements ent vFile lib
                                                            let sts = removeSomeStatements $ adjustStatements sts'
                                                            return (List.map (\s -> (stkind s, stname s)) sts)

printCoqSymbols :: [CoqTerm] -> IO [String]
printCoqSymbols [] = return []
printCoqSymbols (ct:cts) = do
                             mgct <- generateUnresolvedFile ct Map.empty []
                             fns <- printCoqSymbols cts
                             case mgct of
                               Nothing -> return fns
                               Just (_, fn) -> return (fn:fns)  

extractStatements :: [String] -> IO [PreStatementWithList]
extractStatements fns = do
                          newfs' <- mapM (\fn -> extractSymbols fn >>= printCoqSymbols) fns
                          let newfs = Prelude.concat newfs'
                          extractStatements0 newfs [] [] []


unfoldUses :: [PreStatementWithList] -> [PreStatementWithList] -> PreTheoryWithList -> IO [PreStatementWithList]
unfoldUses [] acct _ = return acct
unfoldUses (t:ts) acct thm = do
                         putStrLn $ "Unfolding " ++ (DT.unpack $ sname $ stname t) ++ ", remaining " ++ (show $ List.length ts)
                         let b = List.notElem t acct 
                         let isExtractable u = (fst u /= SelfReference) && (fst u /= BoundVariable) && (fst u /= LocalConstructor)
                         let usm = catMaybes $
                                          List.map (\u -> if isExtractable u then Map.lookup (snd u) thm else Nothing) (stuses t)
                         if b then                             
                              unfoldUses (usm ++ ts) (t:acct) thm
                         else do
                                putStrLn "skipping..."
                                unfoldUses ts acct thm

unfoldUsesList :: [PreStatementWithList] -> PreTheoryWithList -> IO [[PreStatementWithList]]
unfoldUsesList sts th = mapM (\s -> unfoldUses [s] [] th) sts 

ordStatement :: PreStatementWithList -> PreStatementWithList -> Common.PartOrdering
{--ordStatement s1 s2 = if (List.elem (stname s1) (List.map snd $ stuses s2)) then Common.PLT else
                     if (List.elem (stname s2) (List.map snd $ stuses s1)) then Common.PGT else
                     if (stname s1 == stname s2) then Common.PEQ else
                     Common.PNC
--}

ordStatement s1 s2 = if (Map.member (stname s1) $ Map.fromList (List.map (\u -> (snd u, True)) $ stuses s2)) then Common.PLT else
                     if (Map.member (stname s2) $ Map.fromList (List.map (\u -> (snd u, True)) $ stuses s1)) then Common.PGT else
                     if (stname s1 == stname s2) then Common.PEQ else
                     Common.PNC

extractTerms :: [String] -> [PreStatementWithList] -> IO [[[PreStatementWithList]]]
extractTerms [] _ = return []
extractTerms (t:ts) sts = do
                          et <- do
                                  let fsts = List.filter (\s -> (sname $ stname s) == (DT.pack t)) sts 
                                  let m = Map.fromList $ List.map (\s -> (stname s, s)) sts
                                  r <- unfoldUsesList fsts m 
                                  return $ List.map (Common.partsort stname ordStatement) r                                  
                          r <- extractTerms ts sts 
                          return $ et:r

extractTermsCode :: [String] -> [PreStatementWithList] -> IO [[Text]]
extractTermsCode ts sts = do
                          extss <- extractTerms ts sts
                          putStrLn $ "Length of extracted terms: " ++ (show $ List.map (List.map (\ext -> List.length ext)) extss)
                          return $ List.map (List.map (\ext -> DT.concat $ List.map (\s -> DT.append (fromCode $ stcode s)
                                                      $ DT.append "\n(* end " $ DT.append (fqStatementName $ stname s) " *)\n\n") ext)) extss
