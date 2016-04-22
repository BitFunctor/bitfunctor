{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes  #-}

module Network.BitFunctor.Theory.Types where

import Data.Text
import Data.Char
import Data.Foldable
import Data.Monoid
import Control.Monad

import Network.BitFunctor.Common

class (PartOrd a) where
    partCompare :: a -> a -> PartOrdering

class (Codeable a) where
    toText :: a -> Text
    fromText :: Text -> a

class (Nameable a) where
    toPrefix :: Char -> a -> Text
    toSuffix :: a -> Text

toFQName :: Nameable a => Char -> a -> Text
toFQName c x = (toPrefix c x) <> (singleton c) <> (toSuffix x)

class NamedPartable x where
    toParts :: forall a m . (Monad m, Foldable m) => x -> m a
    toNamedParts :: forall b m . (Monad m, Foldable m, Nameable b) => x -> m b

type CodeA t a b = Either a (t (Either a b))

instance (Codeable a, Codeable b) => Codeable (Either a b) where
    fromText t = Left (fromText t)
    toText (Left x) = toText x
    toText (Right x) = toText x

instance (Codeable a, Codeable b, Foldable t) => Codeable (CodeA t a b) where
    fromText t = Left (fromText t)
    toText (Left x) = toText x
    toText (Right mab) = Data.Foldable.foldl (\acc c -> acc <> (toText c)) empty mab

instance NamedPartable (CodeA m a b)  where
    toParts (Left x) = fail "Partable: no parts in raw code"
    toParts (Right x) = x
    toNamedParts (Left x) = fail "Partable: no named parts in raw code"    
    toNamedParts (Right mab) = Data.Foldable.foldl (\acc ab -> case ab of
                                                                 Left x -> acc
                                                                 Right y -> acc <> y) mempty mab

{--
data (Nameable a, Nameable d, Foldable t) =>
                                 StatementA a b c t d e f =
                                 StatementA { stname   :: a
                                            , stkind   :: b 
                                            , stcode   :: CodeA c t d
                                            , stsource :: e -- source filename isomorphism
                                            , stuses   :: t d } 

data TheoryA a b c t d e = TheoryA { stlist :: [StatementA a b c t d e] }
--}

