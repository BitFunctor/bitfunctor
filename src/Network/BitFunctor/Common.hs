module Network.BitFunctor.Common  where

import qualified Data.List as List
import Data.Char (isSpace)

spanEnd :: (a -> Bool) -> [a] -> ([a], [a])
spanEnd p l = let (l1,l2) = List.span p $ List.reverse l in
              (List.reverse l2, List.reverse l1)

headWithDefault :: a -> [a] -> a
headWithDefault x [] = x
headWithDefault _ (h:_) = h

removeStartFromString :: String -> String -> String
removeStartFromString [] s = s
removeStartFromString p [] = []
removeStartFromString pat@(p:pats) str@(s:strs) = if (isSpace p) then
                                                     removeStartFromString pats str
                                                  else if (isSpace s) then
                                                     removeStartFromString pat strs
                                                  else if (p == s) then
                                                     removeStartFromString pats strs
                                                  else str

removeEndFromString :: String -> String -> String
removeEndFromString pat str = List.reverse $ removeStartFromString (List.reverse pat) (List.reverse str)
