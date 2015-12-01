module Network.BitFunctor.Theory.Complexity where

import Network.BitFunctor.Theory.Types
import qualified Data.Map as Map

statementComplexity :: Statement -> Int
statementComplexity a = case (kind a) of
                          Function -> 1
                          Type     -> 1
                          Theorem  -> 1

theoryComplexity :: Theory -> Int
theoryComplexity =  Map.foldr (\a c -> c + (statementComplexity a)) 0
