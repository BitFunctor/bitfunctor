{-# LANGUAGE TypeSynonymInstances, BangPatterns #-}

module Network.BitFunctor.Common where

import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Char (isSpace)

import Data.Binary (Binary (..))
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (POSIXTime, utcTimeToPOSIXSeconds, posixSecondsToUTCTime)



spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p l = let (l1,l2) = List.span p $ List.reverse l in
              (List.reverse l2, List.reverse l1)

headWithDefault :: a -> [a] -> a
headWithDefault x [] = x
headWithDefault _ (h:_) = h

tailWithDefault :: [a] -> [a] -> [a]
tailWithDefault x [] = x
tailWithDefault _ (_:hs) = hs

listinize :: [a] -> [[a]]
listinize = List.map (\x -> [x])

lastWithDefault :: a -> [a] -> a
lastWithDefault x [] = x
lastWithDefault _ l = List.last l

removeStartFromString :: String -> String -> Maybe String
removeStartFromString "" s = Just s
removeStartFromString p "" = Just ""
removeStartFromString pat@(p:pats) str@(s:strs) = if (isSpace p) then
                                                     removeStartFromString pats str
                                                  else if (isSpace s) then
                                                     removeStartFromString pat strs
                                                  else if (p == s) then
                                                     removeStartFromString pats strs
                                                  else Nothing

removeEndFromString :: String -> String -> Maybe String
removeEndFromString pat str = removeStartFromString (List.reverse pat) (List.reverse str) >>= \s -> return $ List.reverse s 

removeStartFromText t1 t2 = removeStartFromString (Text.unpack t1) (Text.unpack t2) >>= \s -> return $ Text.pack s
removeEndFromText t1 t2 = removeStartFromText (Text.reverse t1) (Text.reverse t2) >>= \s -> return $ Text.reverse s

--------------part sorting utils

-- foldl :: (a -> b -> a) -> a -> [b] -> a

data PartOrdering = PLT | PEQ | PGT | PNC


changehead0 :: (a -> Bool) -> [a] -> [a] -> [a]
changehead0 _ l [] = l
changehead0 f l (y:ys) = if (f y) then (y:l)++ys
                                  else changehead0 f (l++[y]) ys

changehead :: (a -> Bool) -> [a] -> [a]
changehead f l = changehead0 f [] l

partsort' :: Ord b => (a -> b) -> [(b, (a, (Integer, [a])))] -> [a] -> [a] 
partsort' id l accl = let !ls = List.sortBy (\(_, (_, (r1, _))) (_, (_, (r2, _))) -> compare r1 r2) l in
                      case ls of
                        [] -> Prelude.reverse accl
                        (x:xs) -> partsort' id xs' ((fst $ snd x):accl) where
                               !xs' = let m = Map.fromList xs in
                                     let majl = snd $ snd $ snd x in
                                     Map.toList $ List.foldl (\m' y -> Map.adjust (\(x, (rx, mx)) -> (x, (rx-1, mx))) (id y) m') m majl    

partsort :: Ord b => (a -> b) -> (a -> a -> PartOrdering) -> [a] -> [a]
partsort _ _ [] = []
partsort id ord l = let l' = List.map (\x -> (id x, (x, List.foldl (\(r,m) y -> case (ord x y) of
                                                                                  PGT -> (r+1, m)
                                                                                  PLT -> (r, y:m)
                                                                                  _ -> (r,m) ) (0,[]) l))) l in                    
                    partsort' id l' [] 



newtype UTCTimeAsPOSIXSeconds = UTCTimeAsPOSIXSeconds UTCTime

instance Binary UTCTimeAsPOSIXSeconds where
  put (UTCTimeAsPOSIXSeconds utc) = do
    put $ utcTimeToPOSIXSeconds utc
  get = do
    seconds <- get
    return . UTCTimeAsPOSIXSeconds $ posixSecondsToUTCTime seconds

instance Binary POSIXTime where
  put = put . toRational
  get = do
    t <- get
    return $ fromRational t

