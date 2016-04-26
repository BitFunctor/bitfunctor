{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies  #-}

module Network.BitFunctor.Theory.Types where

import Data.Text
import Data.Char
import Data.Foldable
import Data.Monoid
import Control.Monad
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Serialize as Serz
import qualified Data.Text.Encoding as TE

import Network.BitFunctor.Common

-- remove from here
class (PartOrd a) where
    partCompare :: a -> a -> PartOrdering

instance Serz.Serialize Text where
  put txt = Serz.put $ TE.encodeUtf8 txt
  get     = fmap TE.decodeUtf8 Serz.get


-- can be statement code
class (Codeable a) where
    toText :: a -> Text
    fromText :: Text -> a
    isFQExtractable :: a -> Bool
    isTheoriable :: a -> Bool
    isSelfReference :: a -> Bool

class (Ord k, Eq k) => Keyable a k | a -> k where
    toKey :: a -> k


-- can be statement name
class (Keyable a k) => Nameable a k where
    toPrefix :: Char -> a -> Text
    toSuffix :: a -> Text
    fromSuffix :: Text -> a
    toFQName :: Char -> a -> Text
    toFQName c x = let p = toPrefix c x in
                   let s = toSuffix x in 
                   if Data.Text.null p then s                                    
                                       else if Data.Text.null s then p
                                            else p <> (singleton c) <> s
    changeNameWith:: (k -> k -> Bool) -> k -> k -> a -> a
    changeNameWith f kf kt x = if (f kf $ toKey x) then fromKey kt x else x
    changeNameWithKey :: k -> k -> a -> a
    changeNameWithKey = changeNameWith (==)
    fromKey :: k -> a -> a
 
type CodeA a b = Either a [Either a b]

toTextWithMap :: (Codeable a, Codeable b) => (b -> Text) -> CodeA a b -> Text
toTextWithMap _ (Left x) = toText x
toTextWithMap f (Right mab) = List.foldl (\acc c -> acc <> (case c of
                                                             Left x -> toText x
                                                             Right c -> f c)) empty mab


fromCodeA :: (Codeable b, Nameable b k, Codeable a) => Char -> CodeA a b -> Text
fromCodeA c = toTextWithMap $ \ct -> if (isFQExtractable ct) then toFQName c ct
                                                             else toText ct

instance (Codeable a, Codeable b) => Codeable (Either a b) where
    fromText t = Left (fromText t)
    toText (Left x) = toText x
    toText (Right x) = toText x
    isFQExtractable (Left x) = False
    isFQExtractable (Right x) = isFQExtractable x
    isTheoriable (Left x) = False
    isTheoriable (Right x) = isTheoriable x
    isSelfReference (Left x) = False
    isSelfReference (Right x) = isSelfReference x

instance Codeable a => Codeable [a] where
    fromText t = []    
    toText l = List.foldl (\acc c -> acc <> (toText c)) empty l
    isFQExtractable l = False
    isTheoriable l = False
    isSelfReference l = False

class (Serz.Serialize s, Eq s, Nameable a k, Codeable c, Codeable c', Nameable c' a, PartOrd s) =>
                                       StatementC a k c c' s | s -> a, s -> c, s -> c' where
    toStatementName :: s -> a
    toStatementCode :: s -> CodeA c c'
    changeStatementCode :: CodeA c c' -> s -> s
    toStatementKey ::  s -> k
    toStatementKey = toKey . toStatementName
 
class (Serz.Serialize t, StatementC a k c c' s) => TheoryC a k c c' s t | t -> s where
    toStatementList :: t -> [s]
    toStatementMap ::  t -> Map.Map k s
    toStatementMap x = let sts = toStatementList x in
                       let ksts = List.map (\s -> (toStatementKey s, s)) sts in
                       Map.fromList ksts


