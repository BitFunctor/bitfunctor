 {-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, ScopedTypeVariables #-}

module Network.BitFunctor.Theory.RESTServer where

import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Acid as Acid
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


showStaticRaw s  = WS.html $ renderHtml $ [shamlet|
                                         <html>
                                            <head>                                                                                              
                                            <body>#{s}
                                       |]

show404 =  [shamlet|                                   
              <p>Error 404. Link not found.
             |]


runTheoryRestServer port acidTh = WS.scotty port $ do
             WS.get "/stnum" $ do
                                 sz <- liftIO $ Acid.query acidTh (ThSize)
                                 showStaticRaw $ "Total " ++ (show sz) ++ " elements."
             WS.get "/statement/:stname" $ do
                                          (stn :: Text.Text) <- WS.param "stname"
                                          str <- liftIO $ Acid.query acidTh (GetStatement stn)
                                          case str of
                                            AcidResultSuccess st ->  let lines = Text.replace "\n" "<br>" (Text.pack $ show st) in
                                                                     WS.html $ LText.pack $ Text.unpack
                                                                     $ mconcat $ ["<html><head>", "</head>", "<body>", "<pre>"] ++ [lines] ++ ["</pre></body></html>"]
                                            AcidResultError -> WS.html $ "Statement not found"
             WS.get "/extracterm/:termname" $ do
                                          (tn :: Text.Text) <- WS.param "termname"
                                          rtcode <- liftIO $ Acid.query acidTh (ExtractTerm tn)
                                          case rtcode of
                                            AcidResultSuccess tcode ->  let lines = Text.replace "\n" "<br>" (Text.intercalate "--------------\n" tcode) in
                                                                        WS.html $ LText.pack $ Text.unpack
                                                                        $ mconcat $ ["<html><head>", "</head>", "<body>", "<pre>"] ++ [lines] ++ ["</pre></body></html>"]
                                            AcidResultError -> WS.html $ "Term not found"
             WS.post "/requestdb" $ do
                                      (req :: CoqTheoryAcidQuery) <- WS.jsonData
                                      case req of
                                        InsertKeyC r -> (liftIO $ Acid.update acidTh r) >>= WS.json
                                        LookupKeyC r -> (liftIO $ Acid.query acidTh r) >>= WS.json
                                        ThSizeC r -> (liftIO $ Acid.query acidTh r) >>= WS.json
                                        InitTheoryFromListC r -> (liftIO $ Acid.update acidTh r) >>= WS.json 
                                        ExtractTermC r -> (liftIO $ Acid.query acidTh r) >>= WS.json
             WS.notFound $ do
                             WS.status HTT.notFound404
                             showStaticRaw show404
