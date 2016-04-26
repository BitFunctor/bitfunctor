{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Console.ArgParser
import Control.Applicative
import Network.BitFunctor.Theory.Types
import Network.BitFunctor.Theory.Coq.Types
import qualified Network.BitFunctor.Theory.Extraction as TE
import qualified Network.BitFunctor.Theory.Coq.Extraction.FileExtraction as FE
import qualified Network.BitFunctor.Theory.Coq.Extraction.Constants as Constants
import qualified Network.BitFunctor.Identifiable as Ident
import qualified Network.BitFunctor.Common as Common
import Network.BitFunctor.Crypto.Hash.Types (toString)

-- import qualified Network.BitFunctor.Crypto.Hash as Hash

import System.Environment
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as Text
import Data.List.Split (splitOneOf)
import Control.Monad
import Data.Text.IO as DTIO (writeFile)
import qualified Data.Time as Time (getCurrentTime)
import qualified Data.Serialize as DS (decode, encode)
import qualified Data.ByteString as BS (writeFile, readFile)

data ExtractorArguments = ExtractorArguments [FilePath] FilePath FilePath [Text.Text]
                          deriving (Show) 

xtractorArgsParser :: ParserSpec ExtractorArguments
xtractorArgsParser = ExtractorArguments
  `parsedBy` optFlagArgs [] "f" [] (\b a -> b ++ [a])  `Descr` "vernac files to process"
  `andBy` optFlag "" "wt" `Descr` "write to theory file"
  `andBy` optFlag "" "rt" `Descr` "read from theory file"   
  `andBy` optFlagArgs [] "e" [] (\b a -> b ++ [Text.pack a])  `Descr` "statements to extract"  
  

xtractorArgsInterface :: IO (CmdLnInterface ExtractorArguments)
xtractorArgsInterface =
  (`setAppDescr` "Coqxtractor test program as a part of Bitfunctor environment")
  <$> (`setAppEpilog` "BitFunctor team")
  <$> mkApp xtractorArgsParser


writeTheoryFile :: [CoqStatementT] -> FilePath -> IO ()
writeTheoryFile _ "" = return ()
writeTheoryFile th thFile = do
                    putStrLn "Writing extracted theory to the file..."
                    let bsw = DS.encode th
                    BS.writeFile thFile bsw


readTheoryFile :: [CoqStatementT] -> FilePath -> IO [CoqStatementT]
readTheoryFile pt "" = return pt
readTheoryFile _ thFile = do
                           putStrLn "Reading the theory from the file..."
                           bsr <- BS.readFile thFile
                           let rthl = DS.decode bsr
                           case rthl of
                             Left s -> do
                                       putStrLn $  "Error in decoding: " ++ s
                                       return []
                             Right th -> return th


writeExtractedTerms :: [Text.Text] -> [CoqStatementT] -> IO()
writeExtractedTerms [] _ = return ()
writeExtractedTerms terms th = do
                     putStrLn "Extracting terms..."
                     ec <- TE.extractTermsCode terms th
                     let extractedCodes = Prelude.concat ec       
                     forM_ extractedCodes (\t -> do
                                            date <- Time.getCurrentTime -- "2008-04-18 14:11:22.476894 UTC"
                                            let sename = "Ex" ++ Constants.generatedFilePrefix ++ (toString $ Ident.id $ Ident.ByBinary $ show date) ++ Constants.vernacFileSuffix 
                                            DTIO.writeFile sename t)
                     putStrLn "Terms have been processed."

extractCoqLibraries :: [FilePath] -> [CoqStatementT] -> IO [CoqStatementT]
extractCoqLibraries [] th = return th
extractCoqLibraries libs th = do
                               putStrLn "Processing libraries..."
                               th' <- FE.extractStatements libs th
                               putStrLn "Libraries have been processed."
                               return th'

checkTheory :: [CoqStatementT] -> IO()
checkTheory [] = return ()
checkTheory th = do
                   putStrLn "Checking the theory..."
                   TE.checkTheoryConsistancy th
                   TE.checkTheoryAcyclessness th
                   putStrLn "The theory is checked."

doCoqExtracting (ExtractorArguments libs thWFile thRFile terms) = do
                                                          th' <- readTheoryFile [] thRFile
                                                          checkTheory th'

                                                          th <- extractCoqLibraries libs th'

                                                          checkTheory th
                                                          writeTheoryFile th thWFile
                                                          
                                                          writeExtractedTerms terms th
               

main = do
  interface <- xtractorArgsInterface
  runApp interface doCoqExtracting      
