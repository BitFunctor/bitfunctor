{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
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
import Network.BitFunctor.Theory.Coq.TheoryAcid

-- import qualified Network.BitFunctor.Crypto.Hash as Hash

import System.Environment
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Text as Text
import Data.Text.Lazy (toStrict)
import qualified Data.Aeson as DA
import Data.Aeson.Parser (json)
import Data.List.Split (splitOneOf)
import Control.Monad
import qualified Data.Text.IO as DTIO (writeFile)
import qualified Data.Time as Time (getCurrentTime)
import qualified Data.ByteString.Lazy.Char8 as BS (writeFile, readFile, putStrLn)
import qualified Data.Text.Lazy.Encoding as LEnc
import qualified Codec.Compression.Zlib as ZL
import Data.Binary
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Status  (statusCode)
import Data.Conduit (($$))
import Data.Conduit.Attoparsec (sinkParser)
import Network.HTTP.Client.Conduit (bodyReaderSource)



data ExtractorArguments = ExtractorArguments [FilePath] [Text.Text] FilePath FilePath [Text.Text]
                          deriving (Show) 

xtractorArgsParser :: ParserSpec ExtractorArguments
xtractorArgsParser = ExtractorArguments
  `parsedBy` optFlagArgs [] "f" [] (\b a -> b ++ [a])  `Descr` "vernac files to process"
  `andBy` optFlagArgs [""] "skip-checks" [] (\b a -> b ++ [Text.pack a]) `Descr` "skip particular check, or empty for all"
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
                    BS.writeFile thFile $ encode th
                    --encodeFile thFile th
                    -- BS.writeFile thFile bsw


readTheoryFile :: [CoqStatementT] -> FilePath -> IO [CoqStatementT]
readTheoryFile pt "" = return pt
readTheoryFile _ thFile = do
                           putStrLn "Reading the theory from the file..."
                           -- bsr <- BS.readFile thFile
                           th <- decodeFile thFile
                           return th
                           {--case rthl of
                             Left s -> do
                                       putStrLn $  "Error in decoding: " ++ s
                                       return []
                             Right th -> return th--}


writeExtractedTerms' :: [Text.Text] -> [CoqStatementT] -> IO()
writeExtractedTerms' [] _ = return ()
writeExtractedTerms' terms th = do
                     putStrLn "Extracting terms..."
                     let ec = TE.extractTermsCode th terms 
                     let extractedCodes = Prelude.concat ec       
                     forM_ extractedCodes (\t -> do
                                            date <- Time.getCurrentTime -- "2008-04-18 14:11:22.476894 UTC"
                                            let sename = "Ex" ++ Constants.generatedFilePrefix ++ (toString $ Ident.id $ Ident.ByBinary $ show date) ++ Constants.vernacFileSuffix 
                                            DTIO.writeFile sename t)
                     putStrLn "Terms have been processed."

writeExtractedTerms :: [Text.Text] -> IO()
writeExtractedTerms []  = return ()
writeExtractedTerms (t:ts) = do
                     putStrLn "Extracting terms from theoryd..."
                     manager <- newManager tlsManagerSettings
                     let requestObject = ExtractTermC $ ExtractTerm t            
                     initialRequest <- parseUrl "http://localhost:2718/requestdb"
                     let request = initialRequest { method = "POST"
                                                   , requestBody = RequestBodyLBS $ DA.encode requestObject
                                                   , requestHeaders = [ ("Content-Type", "application/json; charset=utf-8")] }                     
                     withResponse request manager $ \response -> do
                             putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
                             jcodes <- bodyReaderSource (responseBody response)
                                      $$ sinkParser json
                             let (rcodes :: DA.Result (AcidResult [Text.Text])) = DA.fromJSON jcodes
                             case rcodes of
                                DA.Error s -> putStrLn $ "Error in parsing JSON: " ++ s
                                DA.Success AcidResultError -> putStrLn $ "The term is not found"
                                DA.Success (AcidResultSuccess codes) -> forM_ codes (\t -> do
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

-- skip particular checks not implemented
checkTheory :: [Text.Text] -> [CoqStatementT] -> IO()
checkTheory _ [] = return ()
checkTheory [] _ = return ()
checkTheory _ th = do
                   putStrLn "Checking the theory..."
                   TE.checkTheoryConsistancy th
                   TE.checkTheoryAcyclessness th
                   putStrLn "The theory is checked."

doCoqExtracting (ExtractorArguments libs exclChecks thWFile thRFile terms) = do
                                                          th' <- readTheoryFile [] thRFile
                                                          checkTheory exclChecks th'

                                                          th <- extractCoqLibraries libs th'

                                                          checkTheory exclChecks th
                                                          writeTheoryFile th thWFile
                                                          
                                                          writeExtractedTerms terms
               

main = do
  interface <- xtractorArgsInterface
  runApp interface doCoqExtracting      
