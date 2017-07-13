 {-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, ScopedTypeVariables #-}

module Main where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Binary as Bin
import qualified Data.Acid as Acid
import qualified Data.Map.Strict as Map
import System.Console.ArgParser
import Control.Applicative

import Network.BitFunctor.Theory.Types
import Network.BitFunctor.Theory.Coq.Types
import Network.BitFunctor.Theory.Coq.TheoryAcid
import Network.BitFunctor.Theory.RESTServer


data TheoryDArguments = TheoryDArguments FilePath FilePath 
                        deriving (Show) 

theoryd_portnumber = 2718

theorydArgsParser :: ParserSpec TheoryDArguments
theorydArgsParser = TheoryDArguments
  `parsedBy` reqPos "thdir" `Descr` "theory DB directory"
  `andBy` optFlag "" "rt" `Descr` "read from theory file"     

theorydArgsInterface :: IO (CmdLnInterface TheoryDArguments)
theorydArgsInterface =
  (`setAppDescr` "Theory daemon program as a part of Bitfunctor environment")
  <$> (`setAppEpilog` "BitFunctor team")
  <$> mkApp theorydArgsParser

readTheoryFile :: FilePath -> IO [CoqStatementT]
readTheoryFile "" = return []
readTheoryFile thFile = do
                         putStrLn "Reading the theory from the file..."                           
                         th <- Bin.decodeFile thFile
                         return th

doTheoryDaemon (TheoryDArguments thDir thRFile) = do
          th' <- readTheoryFile thRFile                         
          acidTh <- Acid.openLocalStateFrom thDir (CoqTheoryAcid Map.empty)

          if (Prelude.null th') then return ()
          else do                    
            putStrLn $ "Initializing theory from file..."
            res <- Acid.update acidTh (InitTheoryFromList th')
            return ()
             
          (AcidResultSuccess sz) <- Acid.query acidTh (ThSize)
          putStrLn $ "Theory DB is open, total " ++ (show sz) ++ " elements."

          runTheoryRestServer theoryd_portnumber acidTh

          Acid.closeAcidState acidTh

main = do
  interface <- theorydArgsInterface
  runApp interface doTheoryDaemon
