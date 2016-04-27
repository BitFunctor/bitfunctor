{-# LANGUAGE AllowAmbiguousTypes #-}

module Network.BitFunctor.Theory.Complexity where

import Network.BitFunctor.Theory.Types
import qualified Data.List as List

statementComplexity :: StatementC a k c c' s => s -> Int
statementComplexity a = 0

theoryComplexity :: TheoryC a k c c' s t => t -> Int
theoryComplexity t = 0 -- List.foldr (\a c -> c + (statementComplexity a)) 0 $ toStatementList t
