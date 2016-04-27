{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.BitFunctor.Theory.Coq.Extraction.FileExtraction  where

import System.Process
import System.Exit
import System.IO
import qualified System.Directory as SD (doesFileExist)

import Control.Monad (forM_, mapM)
import Data.Monoid ((<>))
import Control.Lens
import Data.Text as DT
import qualified Data.Text.IO as TextIO (writeFile)
import qualified Data.Text.Encoding as TE
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.List as List 
import qualified Data.ByteString as DBS
import qualified Data.ByteString.Char8 as DBS8
import qualified Data.Map as Map
import Data.Foldable (foldlM)
import qualified Data.Time as Time (getCurrentTime)
import Data.Tuple (swap)
import qualified Text.ParserCombinators.Parsec as PS (parse)

import Network.BitFunctor.Crypto.Hash.Types
import Network.BitFunctor.Theory.Types
import Network.BitFunctor.Theory.Coq.Types
import qualified Network.BitFunctor.Common as Common
import qualified Network.BitFunctor.Crypto.Hash as Hash
import qualified Network.BitFunctor.Theory.Extraction as Extraction
import qualified Network.BitFunctor.Theory.Coq.Extraction.Constants as Constants
import qualified Network.BitFunctor.Theory.Utils as Utils
import qualified Network.BitFunctor.Identifiable as Ident
import qualified Network.BitFunctor.Theory.Coq.Extraction.GlobFileParser as GP

-- local aliases
type CoqTermEntry = (CoqTerm, [GP.GlobFilePosition])
type PreStatementWithPositions = CoqStatementA CoqTermEntry
type PreTheoryWithPositions = PreCoqTheory PreStatementWithPositions


-- if the final term is stripped we could lose the shift number for dependencies
-- so it will not be stipped, always having newline as the first symbol - and that is OK only for Printed term
-- if to take from the user code some extra spaces could be added to the final Code

loadVernacCode :: FilePath -> Int -> Maybe Int -> IO (Int, CoqCode)
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
                                        return $ (pos1 - n, Left $ DT.dropWhileEnd (\c -> c /= Constants.coqStatementDelimiter)
                                                                 $ TE.decodeUtf8 bs)
                                     else
                                        return $ (0, Left "")
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
                              let sn = -- DT.takeWhileEnd (\c -> c /= Constants.coqModuleDelimiter) $
                                       DT.pack $ GP.ename r in
                              let csn = CoqStatementName (DT.pack ln) (DT.pack mn) sn in 
                              if (DT.null sn) then Nothing
                                  else                                       
                                    Just (GP.epos r, CoqStatementA csn (GP.ekind r) (Left "") (Ident.id $ Ident.ByBinary lib) [((SelfReference, csn),[GP.epos r])])
                           StopStatement -> Nothing
                           IgnorableRes -> Nothing

insertToList :: Ord a =>  (b -> b -> b) -> a -> b -> [(a, b)] -> [(a, b)]
insertToList f k x l = Map.toList $ Map.insertWith f k x $ Map.fromList l

-- TODO: do not collect Code when firstly process user file
-- the code collection should be done only for Printed terms
collectStatements0 :: [GP.GlobFileEntry] -> FilePath -> GP.GlobFileName -> Maybe (GP.GlobFilePosition, PreStatementWithPositions) ->
                      Maybe Int -> [PreStatementWithPositions] -> IO [PreStatementWithPositions]
collectStatements0 [] vfname _ (Just (GP.GlobFilePosition pos1 _, cs)) mpos2  accs = do
                                                              (n, cd) <- loadVernacCode vfname pos1 mpos2  
                                                              let cs' = stcode .~ cd $
                                                                        stuses.mapped._2.mapped %~ (GP.adjustGlobFilePosition (-n)) $ cs
                                                              return $ cs':accs 
-- List.map (\u -> (fst u, List.map (adjustGlobFilePosition (-n)) $ snd u)) (stuses cs)}:accs
collectStatements0 [] _ _ Nothing _  accs = return accs
collectStatements0 (s:ss) vfname libname pcs@(Just jpcs@(GP.GlobFilePosition pos1 _, cs)) mpos2 accs = 
            case s of
                GP.GlobFileStatement r-> do                                      
                                      case (GP.ekind r, cs^.stkind) of
                                        (Constructor, Inductive) ->                                              
                                             let pcons' = fromGlobFileRawEntry libname r in
                                             case pcons' of
                                               Nothing -> fail "Cannot collect constructor"
                                               Just (pos, cons') -> 
                                                 let cons = cons' in
                                                 let jpcs' = Just $ _2.stuses %~ (insertToList (++) (LocalConstructor, cons'^.stname) [pos])
                                                                  $ jpcs in  
                                                 collectStatements0 ss vfname libname jpcs' Nothing (cons:accs)
                                        (Constructor, _) -> fail "Meeting constructor when collecting not-Inductive"
                                        _ -> do
                                               (n, cd) <- loadVernacCode vfname pos1 (Just $ fromMaybe ((GP.espos $ GP.epos r) - 1)  mpos2)
                                               let cs' = stcode .~ cd $
                                                         stuses.mapped._2.mapped %~ (GP.adjustGlobFilePosition (-n)) $ cs
                                               let accs' = cs':accs
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
                                                      let newpcs = Just (GP.GlobFilePosition pos1 pos1,
                                                                        stuses %~ (insertToList (++) (_stkind rs, _stname rs) [pos]) $ cs) in
                                                      collectStatements0 ss vfname libname newpcs Nothing accs
                                         IgnorableRes -> collectStatements0 ss vfname libname pcs Nothing accs
collectStatements0 (s:ss) vfname libname Nothing _ accs =
            case s of
                GP.GlobFileStatement r ->  let newpcs = fromGlobFileRawEntry libname r in
                                        collectStatements0 ss vfname libname newpcs Nothing accs                     
                GP.GlobFileResource r ->   collectStatements0 ss vfname libname Nothing Nothing accs

collectStatements sts vfname libname = collectStatements0 sts vfname libname Nothing Nothing []


clearCoqGeneratedFiles :: [FilePath] -> IO ()
clearCoqGeneratedFiles [] = return ()
clearCoqGeneratedFiles (f:fs) = return () -- Utils.removeFiles [f++Constants.vernacFileSuffix,
                                --                   f++Constants.vernacBinaryFileSuffix,
                                --                   f++Constants.globFileSuffix] >> clearCoqGeneratedFiles fs


getPrintedStatement :: CoqTerm -> IO (Maybe (DT.Text, DT.Text))
getPrintedStatement (k, sts) =  do
                            let fqstname = toFQName Constants.coqModuleDelimiter sts
                            let mname = sts^.modname
                            let shname = sts^.sname
                            let lname = sts^.libname
                            date <- Time.getCurrentTime -- "2008-04-18 14:11:22.476894 UTC"
                            let sename = Constants.xtrFilePrefix ++ (toString $ Ident.id $ Ident.ByBinary (show date, sts))

                            let fwPname' = Constants.xtrPrintFilePrefix ++ sename
                            let fwIname' = Constants.xtrImplicitFilePrefix ++ sename
                            let fwCname' = Constants.xtrCheckFilePrefix ++ sename

                            let fwPname = fwPname' ++ Constants.vernacFileSuffix
                            let fwIname = fwIname' ++ Constants.vernacFileSuffix
                            let fwCname = fwCname' ++ Constants.vernacFileSuffix

                            putStrLn $ "Generating files for " ++ show k ++ " " ++ (DT.unpack fqstname)

                            TextIO.writeFile fwPname $ Constants.fullPrintTerm "" lname mname fqstname
                            TextIO.writeFile fwIname $ Constants.fullPrintImplicit "" lname mname fqstname
                            TextIO.writeFile fwCname $ Constants.fullPrintCheck "" lname mname fqstname
 
                            (ecP, s1P', _) <- readProcessWithExitCode Constants.coqExecutable [fwPname] []
                            (ecI, s1I', _) <- readProcessWithExitCode Constants.coqExecutable [fwIname] []
                           

                            case (ecP, ecI, Constants.isAbnormallyPrinted k) of
                                (ExitFailure _ , _, _) -> do
                                                             putStrLn ("\x1b[31mError in coqc: \x1b[0m" ++ fwPname)
                                                             clearCoqGeneratedFiles [fwPname', fwCname', fwIname'] 
                                                             return Nothing
                                (ExitSuccess , ExitFailure _ , True) -> do
                                                             putStrLn ("\x1b[31mError in coqc: \x1b[0m" ++ fwIname)
                                                             clearCoqGeneratedFiles [fwPname', fwCname', fwIname'] 
                                                             return Nothing
                                _ -> do
                                        let s1P = DT.pack s1P'
                                        let s1I = DT.pack s1I'
                                        

                                        let gfilename = sts^.libname
                                        let gmodname = sts^.modname
                                        -- Require Export "MainLib"                                                   
                                        let header = (Constants.coqExportLib gfilename) <> (Constants.coqImportMod gmodname)
                                        -- get a term definition without name and with type at the end
                                        let prebody = DT.strip $ Prelude.head $ DT.splitOn Constants.coqPrintCommentsDelimiter s1P
                                        -- get a short name and type as a list
                                        let firstword = Prelude.head $ DT.splitOn Constants.coqSpace prebody

                                        let pretypenameI = DT.splitOn Constants.coqTypeDelimiter $ DT.strip
                                                          $ Prelude.head $ DT.splitOn Constants.coqPrintCommentsDelimiter s1I
                                       

                                        -- retrieve short name
                                        let shortname' = Prelude.head pretypenameI
                                        -- retrieve type name
                                        let typenameI = DT.strip $ DT.intercalate Constants.coqTypeDelimiter $ Prelude.tail pretypenameI
                                        
                                        -- strip the type from the prebody
                                        let typeStrippedBodyI = Common.removeEndFromText (Constants.coqTypeDelimiter <> typenameI)
                                                                $ DT.intercalate Constants.coqPrintEqSign $ Prelude.tail
                                                                $ DT.splitOn Constants.coqPrintEqSign prebody 
                                          
                                        typeStrippedBodyM <- case typeStrippedBodyI of
                                                                   Just bI -> return $ Just $ DT.strip bI
                                                                   Nothing ->  do
                                                                                (ecC, s1C', _) <- readProcessWithExitCode Constants.coqExecutable [fwCname] []
                                                                                case ecC of
                                                                                   ExitFailure _ -> do
                                                                                                      putStrLn ("\x1b[31mError in coqc: \x1b[0m" ++ fwCname)
                                                                                                      return Nothing
                                                                                   ExitSuccess -> let s1C = DT.pack s1C' in
                                                                                                  let pretypenameC = DT.splitOn Constants.coqTypeDelimiter $ DT.strip
                                                                                                                      $ Prelude.head $ DT.splitOn Constants.coqPrintCommentsDelimiter s1C in
                                                                                                  let typenameC = DT.strip $ DT.intercalate Constants.coqTypeDelimiter $ Prelude.tail pretypenameC in
                                                                                                  let typeStrippedBodyC = Common.removeEndFromText (Constants.coqTypeDelimiter <> typenameC)
                                                                                                                          $ DT.intercalate Constants.coqPrintEqSign $ Prelude.tail
                                                                                                                          $ DT.splitOn Constants.coqPrintEqSign prebody  in
                                                                                                   return typeStrippedBodyC 
                                        -- construct the body
                                        -- putStrLn $ "First word: " ++ firstword
                                        -- the hack with firstword is needed in particular for classes, extracted as inductives
                                        -- so when firstword==shortname it cannot be compiled, so must be processed alternatively
                                        -- but otherwise could be used as is
                                        let shortname = DT.takeWhileEnd (\c -> c /= Constants.coqModuleDelimiter) shortname'
                                        let mb =  if k == Axiom || k == Variable then
                                                     Just $ Constants.coqDefineTerm shortname typenameI ""
                                                  else
                                                     if (DT.strip firstword /= DT.strip shortname) then
                                                         Just $ prebody <> (DT.singleton Constants.coqStatementDelimiter)
                                                     else Nothing
                                        let mb' = case mb of
                                                    Nothing -> typeStrippedBodyM >>= \typeStrippedBody -> return $ Constants.coqDefineTerm shortname typenameI typeStrippedBody
                                                    Just _ -> mb
                                        case mb' of                                                                                       
                                          Nothing -> do
                                                       putStrLn $ "\x1b[31mCannot generate term's code\x1b[0m"
                                                       clearCoqGeneratedFiles [fwPname', fwCname', fwIname'] 
                                                       return Nothing
                                          Just body -> do
                                                       clearCoqGeneratedFiles [fwPname', fwCname', fwIname'] 
                                                       return $ Just (header, body)


generateUnresolvedFile :: CoqTerm -> PreCoqTheory a -> PreCoqTheory b -> [(CoqStatementName, FilePath)] -> IO (Maybe (CoqStatementName, FilePath))
generateUnresolvedFile ct@(k, sts) thm thm' filem =
                             if (Map.member sts thm) || (Map.member sts thm') || (Constants.resourceKind k /= Resource) then return Nothing
                             else 
                                   let mf = Map.lookup sts $ Map.fromList filem in
                                   case mf of
                                    Just f -> return Nothing
                                    Nothing -> do
                                        mhb <- getPrintedStatement ct
                                        case mhb of
                                          Nothing -> do
                                                      putStrLn "\x1b[31mCannot generate term body\x1b[0m"
                                                      return Nothing
                                          Just (header, body) -> do
                                                  let mname = sts^.modname
                                                  let fqstname = toFQName Constants.coqModuleDelimiter sts  
                                                  let newst = header <> body                              
                                                  let idChunk = Ident.id $ Ident.ByBinary (sts^.libname, mname, body)
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
                                                                 let thm1 = Map.fromList $ List.map (\ps -> (ps^.stsource, ps^.stname))
                                                                                         $ Map.elems thm
                                                                 let thm2 = Map.fromList $ List.map (\ps -> (ps^.stsource, ps^.stname))
                                                                                         $ Map.elems thm'    
                                                                 let mfile1 = Map.lookup idFile thm1
                                                                 let mfile2 = Map.lookup idFile thm2
                                                                 case (mfile1, mfile2) of
                                                                   (_, Just name2) -> do
                                                                                  putStrLn "- found in the Theory, but file doesn't exist?"
                                                                                  return $ Just (sts, newfn)
                                                                   (Just name1, _) -> do
                                                                                  putStrLn "- found in the Theory, but file doesn't exist?"
                                                                                  return $ Just (sts, newfn)
                                                                   (Nothing, Nothing) -> do
                                                                                let frname = newfn ++ Constants.vernacFileSuffix
                                                                                putStrLn $ DT.unpack $ "Writing chunk for " <> fqstname <> ":\n" <> newst
                                                                                TextIO.writeFile frname newst
                                                                                return $ Just (sts, newfn)
                                

--(a -> b -> a) -> a -> [b] -> a
--(b -> a -> m b) -> b -> t a -> m b
-- all new statements -> theory -> list of (sts, filenames) 
generateUnresolvedFiles :: [CoqTerm] -> PreCoqTheory a -> PreCoqTheory b -> [(CoqStatementName, FilePath)] -> IO [(CoqStatementName, FilePath)]
generateUnresolvedFiles cts thm thm' tsf = do                                     
                                    mfiles <- foldlM (\fm u -> do
                                                               mts <- generateUnresolvedFile u thm thm' (fm ++ tsf)
                                                               case mts of
                                                                 -- already in theory or impossible to generate 
                                                                 Nothing -> return fm
                                                                 Just x -> return $ (x:fm)  
                                                         ) [] cts
                                    -- nub could be omitted if all correct
                                    return mfiles -- $ List.nub mfiles 

-- List.map (List.map fst . _stuses)
generateUnresolvedFilesForDependencies sts thm thm' tsf = generateUnresolvedFiles (List.nub $ Prelude.concat (mapped.mapped %~ fst $ mapped %~ _stuses $ sts)) thm thm' tsf
                                        
-- after generation new files, original lib name should be changed to the filenames generated 
changeTermLib :: Map.Map CoqStatementName FilePath -> CoqTerm -> CoqTerm
changeTermLib m (k,t) = let filen = Map.lookup t m in                        
                        case filen of
                          Nothing -> (k,t)
                          Just fn -> (k, CoqStatementName (DT.pack fn) "" (t^.sname))

-- rename this, removes only constructors for the first stage of user file processing!
removeSomeStatements :: [CoqStatementA a] -> [CoqStatementA a]
removeSomeStatements = List.filter (\s -> s^.stkind /= Constructor)

adjustStatements :: [PreStatementWithPositions] -> [PreStatementWithPositions]
adjustStatements sts =  -- List.nubBy eqStatement .
                    mapped %~ (\s -> stuses.mapped %~ (\u -> let pos = List.nub $ snd u in
                                                  if (fst u == (s^.stkind, s^.stname)) then
                                                       ((SelfReference, s^.stname), pos)
                                                  else
                                                      if (fst . fst) u == Variable then
                                                           ((BoundVariable, snd $ fst u), pos)
                                                      else (fst u, pos)) $ s) $ sts

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
takeFromCode :: CoqCode -> GP.GlobFilePosition -> DT.Text
takeFromCode (Left t) (GP.GlobFilePosition s e) = Utils.takePositionsFromText t s e
takeFromCode (Right _) _ = ""
                    

transformLeftCode :: [CoqTermEntry] -> CoqCode -> CoqCode
transformLeftCode _ (Left "") = Right []
transformLeftCode uslist ct@(Left t) =
       let uslist' = Prelude.concat $ List.map (\u -> List.map (\p -> (fst u, p)) $ snd u) uslist in
       let ussorted = List.sortBy (\u1 u2 -> compare (GP.espos $ snd u1) (GP.espos $ snd u2)) uslist' in
       let (codeparts', p) =
                   List.foldl (\(l,p) u -> if ((GP.espos $ snd u) > p) then
                                                (l ++ [Left $ takeFromCode ct $ GP.GlobFilePosition p ((GP.espos $ snd u) - 1)] ++ [Right $ fst u], (GP.eepos $ snd u) + 1)
                                           else (l ++ [Right $ fst u], (GP.eepos $ snd u) + 1)) ([],0) ussorted in
                  Right $ codeparts' ++ [Left $ takeFromCode ct $ GP.GlobFilePosition p ((DBS.length $ TE.encodeUtf8 t) - 1)]                                   
-- suggest to be never called
transformLeftCode _ (Right pts) = Right pts

transformCoqStatement :: ([a] -> CoqCode -> CoqCode) -> CoqStatementA a -> CoqStatementT
transformCoqStatement tc (CoqStatementA stn stk stc sts stu) = CoqStatementA stn stk (tc stu stc) sts []


-- files to process -> processed files -> map of extracted sts to files -> accumulated list of extracted sts -> final list of extracted sts
extractStatements0 :: [FilePath] -> [FilePath] -> [(CoqStatementName, FilePath)] -> [CoqStatementT] -> [PreStatementWithPositions] -> IO [CoqStatementT]
extractStatements0 [] accf acct th accs = do
                                         putStrLn "Transforming code..."
                                         let tscode = List.map (transformCoqStatement transformLeftCode) accs
                                         putStrLn "Looking for the same codes..."
                                         tsFolded <- Extraction.foldEqualCode $ List.nub $ tscode ++ th
                                         return tsFolded
extractStatements0 (fn:fs) accf acct th accs = do                            
                            if (List.elem fn accf) then do
                                                      putStrLn $ "File already processed: " ++ fn
                                                      extractStatements0 fs accf acct th accs
                            else do
                               let vFile = fn ++ Constants.vernacFileSuffix
                               let gFile = fn ++ Constants.globFileSuffix
                               (ec, s1, _) <- readProcessWithExitCode Constants.coqExecutable [vFile] []
                               case ec of
                                 ExitFailure _ -> do
                                                   putStrLn ("\x1b[31mError in coqc: \x1b[0m" ++ vFile)
                                                   extractStatements0 fs (fn:accf) acct th accs
                                 ExitSuccess ->	 do
                                                   -- putStrLn ("coqc output:\n" ++ s1)
                                                   globfile  <- readFile gFile                                                  
                                                   case (PS.parse GP.globfileData "" globfile) of
                                                       Left err -> do
                                                                     putStrLn "\x1b[31mParse error: \x1b[0m" >> print gFile >> print err
                                                                     extractStatements0 fs (fn:accf) acct th accs
                                                       Right (dig, lib, ent)  -> do
                                                                   sts' <- collectStatements ent vFile lib
                                                                   let sts = adjustStatements sts'
                                                                   let newacc = sts ++ accs
                                                                   let thm = toStatementMap newacc
                                                                   let thm' = toStatementMap th
                                                                   acct' <- generateUnresolvedFilesForDependencies sts thm thm' acct
                                                                   let newacct = acct' ++ acct
                                                                   let newnames = Map.fromList newacct
                                                                   let chacc = mapped.stuses.mapped._1 %~ (changeTermLib newnames) $ newacc 
                                                                   let newacc' = chacc 
                                                                   let newfiles' = List.nub $ (List.map snd acct') ++ fs
                                                                   putStrLn $ "File " ++ fn ++ " has been processed, remaining " ++
                                                                              (show $ List.length newfiles')
                                                                   extractStatements0 newfiles' (fn:accf) newacct th newacc'
                                      

                           

extractSymbols :: FilePath -> IO [CoqTerm]
extractSymbols fn = do
                     let vFile = fn ++ Constants.vernacFileSuffix
                     let gFile = fn ++ Constants.globFileSuffix
                     (ec, s1, _) <- readProcessWithExitCode Constants.coqExecutable [vFile] []
                     case ec of
                        ExitFailure _ -> do
                                          putStrLn ("\x1b[31mError in coqc: \x1b[0m" ++ vFile)
                                          return []
                        ExitSuccess ->	 do                                           
                                           globfile <- readFile gFile                                                  
                                           case (PS.parse GP.globfileData "" globfile) of
                                               Left err -> do
                                                            putStrLn "\x1b[31mParse error: \x1b[0m" >> print gFile >> print err
                                                            return []
                                               Right (dig, lib, ent)  -> do
                                                            sts' <- collectStatements ent vFile lib
                                                            let sts = removeSomeStatements $ adjustStatements sts'
                                                            return (List.map (\s -> (s^.stkind, s^.stname)) sts)

printCoqSymbols :: [CoqTerm] -> IO [FilePath]
printCoqSymbols cts = do
                       cts' <- mapM (\ct -> generateUnresolvedFile ct Map.empty Map.empty []) cts
                       return $ List.map snd $ catMaybes cts'
 

extractStatements :: [FilePath] -> [CoqStatementT] -> IO [CoqStatementT]
extractStatements fns th = do
                          newfs' <- mapM (\fn -> extractSymbols fn >>= printCoqSymbols) fns
                          let newfs = Prelude.concat newfs'
                          -- [FilePath] -> [FilePath] -> [(CoqStatementName, FilePath)] -> [CoqStatementT] -> [PreStatementWithPositions]
                          extractStatements0 newfs [] [] th []


