{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import System.Environment
import Data.Text
import Control.Monad
import qualified Data.ByteString as BS (writeFile, readFile)
import qualified Codec.Compression.Zlib as ZL
import qualified Data.Text.Encoding as TE
import Data.Serialize as DS

-- newtype NewText = NewText Text deriving Show

instance Serialize Text where
  put txt = put $ TE.encodeUtf8 txt
  get     =  fmap TE.decodeUtf8 get 

testText = 6::Int-- NewText "eefefef" -- "#.<>[]'_,:=/\\+(){}!?*-|^~&@\8322\8321\8729\8868\8869"
fileName = "testext.bin"


main = do
        putStrLn "Writing text to the file..."
        let bsw = encode testText
        BS.writeFile fileName bsw
        
        putStrLn "Reading the text from the file..."
        bsr <- BS.readFile fileName
        let tt = decode bsr
        case tt of
             Left s -> do
                        putStrLn $  "Error in decoding: " ++ s
                        return ()
             Right (t::Int) -> do
                        putStrLn $ show t
                        return () 
