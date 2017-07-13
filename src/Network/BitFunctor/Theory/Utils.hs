{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.BitFunctor.Theory.Utils  where

import qualified System.Directory as SD (doesFileExist, removeFile)
import Data.Text
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import qualified Data.String.Utils as SU
import qualified Data.List as List
import Control.Lens

removeFiles :: [FilePath] -> IO ()
removeFiles [] = return ()
removeFiles (f:fs) = do
                    bFileExists <- SD.doesFileExist f
                    if bFileExists then
                       SD.removeFile f
                    else
                       removeFiles fs
                    removeFiles fs

takePositionsFromText :: Text -> Int -> Int -> Text
takePositionsFromText t s e = TE.decodeUtf8
                              $ BS.take (fromIntegral (e-s+1))
                              $ BS.drop (fromIntegral s)
                              $ TE.encodeUtf8 t

groupListBy :: (a -> String) -> (a -> a -> Bool) -> [a] -> IO [[a]]
groupListBy sh _ [] = return []
groupListBy sh f (x:xs) = do
                         let (p1, p2) = List.partition (f x) xs
                         let g = x:p1
                         putStrLn $ "The group for " ++ (sh x) ++ " -> " ++ (SU.join "," $ List.map sh g)
                         gs <- groupListBy sh f p2 
                         return $ g:gs

jumpChar :: (String, String) -> (String, String)
jumpChar (s, []) = (s, [])
jumpChar (s, x:xs) = (s++[x], xs)
  
--  (b -> a -> b) -> b -> t a -> b 
shortenText0 :: [(String, String)] -> [String]
shortenText0 tl = let (tt, txs) = unzip tl in
                  if (List.nub tt == tt) || (List.foldl (&&) True $ mapped %~ List.null $ txs) then tt
                  else shortenText0 $ mapped %~ jumpChar $ tl  
  

shortenText :: [Text] -> [Text]
shortenText tl = mapped %~ pack $ shortenText0 $ List.zip (repeat "") (mapped %~ unpack $ tl) 

removeClosedTextAfterText :: Int -> Text -> Text -> Text
removeClosedTextAfterText _ _ "" = ""
removeClosedTextAfterText 0 pat s = let msp = stripSuffix pat s in
                                    case (msp, Data.Text.last s) of
                                      (Nothing, ')') -> removeClosedTextAfterText (-1) pat $ Data.Text.init s
                                      (Nothing, '(') -> removeClosedTextAfterText 1 pat $ Data.Text.init s
                                      (Nothing, _) -> removeClosedTextAfterText 0 pat $ Data.Text.init s
                                      (Just sp, _) -> sp
removeClosedTextAfterText n pat s = case (Data.Text.last s) of
                                      ')' -> removeClosedTextAfterText (n-1) pat $ Data.Text.init s
                                      '(' -> removeClosedTextAfterText (n+1) pat $ Data.Text.init s
                                      _ -> removeClosedTextAfterText n pat $ Data.Text.init s
                                      


{--
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
--}
