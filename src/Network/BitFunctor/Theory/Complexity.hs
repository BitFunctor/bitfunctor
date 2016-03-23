module Network.BitFunctor.Theory.Complexity where

import Network.BitFunctor.Theory.Types
import qualified Data.Map as Map

statementComplexity :: Statement -> Int
statementComplexity a = 0

theoryComplexity :: Theory -> Int
theoryComplexity =  Map.foldr (\a c -> c + (statementComplexity a)) 0
