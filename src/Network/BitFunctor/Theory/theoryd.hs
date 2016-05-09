 {-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, ScopedTypeVariables #-}

module Main where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Binary as Bin
import qualified Data.Acid as Acid
import qualified Data.Map.Strict as Map
import System.Console.ArgParser
import Control.Applicative

import qualified Web.Scotty as WS
import qualified Network.HTTP.Types as HTT
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Hamlet
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (liftIO)


import Network.BitFunctor.Theory.Types
import Network.BitFunctor.Theory.Coq.Types
import Network.BitFunctor.Theory.Coq.TheoryAcid


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

showStatic s  = WS.html $ renderHtml $ [shamlet|
                                         <html>
                                            <head>                                                                                              
                                            <body>#{s}
                                       |]

show404 =  [shamlet|                                   
              <p>Error 404. Link not found.
             |]


doTheoryDaemon (TheoryDArguments thDir thRFile) = do
          th' <- readTheoryFile thRFile                         
          acidTh <- Acid.openLocalStateFrom thDir (CoqTheoryAcid Map.empty)
          if (Prelude.null th') then  return ()
          else do                    
            putStrLn $ "Initializing theory..."
            Acid.update acidTh (InitTheoryFromList th')
          sz <- Acid.query acidTh (ThSize)
          putStrLn $ "Theory DB is open, total " ++ (show sz) ++ " elements."

          WS.scotty theoryd_portnumber $ do
             WS.get "/stnum" $ do
                                 sz <- liftIO $ Acid.query acidTh (ThSize)
                                 showStatic $ "Total " ++ (show sz) ++ " elements."
             WS.get "/term/:termname" $ do
                                          (tn :: Text.Text) <- WS.param "termname"
                                          tcode <- liftIO $ Acid.query acidTh (ExtractTerm tn)
                                          let lines = Text.replace "\n" "<br>" (mconcat tcode)
                                          WS.html $ LText.pack $ Text.unpack $ mconcat $ ["<html><head>", "</head>", "<body>", "<pre>"] ++ [lines] ++ ["</pre></body></html>"]
             WS.post "/requestdb" $ do
                                      (req :: CoqTheoryAcidQuery) <- WS.jsonData
                                      case req of
                                        InsertKeyC r -> do
                                                           liftIO $ Acid.update acidTh r
                                                           WS.json ("Success" :: Text.Text)
                                        LookupKeyC r -> do
                                                           v <- liftIO $ Acid.query acidTh r
                                                           WS.json v
                                        ThSizeC r -> do
                                                           v <- liftIO $ Acid.query acidTh r
                                                           WS.json v 
                                        InitTheoryFromListC r -> do
                                                           v <- liftIO $ Acid.update acidTh r
                                                           WS.json ("Success" :: Text.Text)
                                        ExtractTermC r -> do
                                                           v <- liftIO $ Acid.query acidTh r
                                                           WS.json v 
             WS.notFound $ do
                             WS.status HTT.notFound404
                             showStatic show404

          Acid.closeAcidState acidTh

main = do
  interface <- theorydArgsInterface
  runApp interface doTheoryDaemon
