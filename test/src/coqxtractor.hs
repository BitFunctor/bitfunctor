module Main where

import Network.BitFunctor.Theory.Types
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

commandLineFlags = ["-e"]

-- t a -> (a -> m b) -> m (t b) 
main = do
       args <- getArgs
       let argsSplitted = splitOneOf commandLineFlags args
       let libs = Common.headWithDefault [] argsSplitted

       th <- FE.extractStatements libs            
       TE.checkTheoryConsistancy th
       TE.checkTheoryAcyclessness th
      
       putStrLn "Libraries have been processed."

       let terms = List.map Text.pack $ Common.headWithDefault [] $ Common.tailWithDefault [[]] argsSplitted
       if  (not $ Prelude.null terms) then do
                                            putStrLn "Extracting terms..."
                                            ec <- TE.extractTermsCode terms th
                                            let extractedCodes = Prelude.concat ec       
                                            forM_ extractedCodes (\t -> do
                                                                 date <- Time.getCurrentTime -- "2008-04-18 14:11:22.476894 UTC"
                                                                 let sename = "Ex" ++ Constants.generatedFilePrefix ++ (toString $ Ident.id $ Ident.ByBinary $ show date) ++ Constants.vernacFileSuffix 
                                                                 DTIO.writeFile sename t)
                                            putStrLn "Terms have been processed"
                                       else return ()



{--
       putStrLn "Writing theory to the file..."
       let bsw = DS.encode extractedStms
       BS.writeFile "theory.btf" bsw
       putStrLn "Reading theory from the file..."
       bsr <- BS.readFile "theory.btf"
       let rthl = DS.decode bsr
       case rthl of
         Left s -> putStrLn $  "Error in decoding: " ++ s
         Right th ->  if (th /= extractedStms) then
                        putStrLn "Writing/reading fails"
                      else
                        putStrLn "Writing/reading is OK"
--}
