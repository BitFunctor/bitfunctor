{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Network.BitFunctor.Theory.Types where

import qualified Data.Map as Map
import Data.ByteString
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Control.Applicative
import Data.Text
import Data.Binary
import qualified Network.BitFunctor.Crypto.Hash as Hash

data Code = CoqText Text
            deriving (Eq, Show, Generic)

data CoqKind = Unknown | Definition | Theorem | Notation | Tactic | Variable | Constructor | Proof | Library | Module | Section | Inductive | Axiom | Scheme | ModType | Instance | SynDef | Class | Record | Projection | Method
            deriving (Eq, Show, Generic)

-- obsolete type, refactoring is needed
data Kind = Type0 | Function0 | Theorem0
            deriving (Eq, Show, Generic)

-- instance Binary Code where
--  put (CoqText a) = put a
--  get = get >>= \a -> return (CoqText a)

{--instance Binary Code where
  put (CoqText a) = put a
  get = get >>= \a -> return (CoqText a)
--}
{-- refactoring needed
instance Binary Kind where
  put Unknown = putWord8 255
  put Definition = putWord8 0
  put Theorem = putWord8 1
  put Notation = putWord8 2
  put Tactic = putWord8 3
  put Variable = putWord8 4
  put Constructor = putWord8 5
  put Proof = putWord8 6
  put Library = putWord8 7
  put Module = putWord8 8
  put Section = putWord8 9
  put Inductive = putWord8 10
  put Axiom = putWord8 11
  put Scheme = putWord8 12
  put ModType = putWord8 13
  put Instance = putWord8 14
  put SynDef = putWord8 15
  put Class = putWord8 16
  put Record = putWord8 17
  put Projection = putWord8 18
  put Method = putWord8 19
  get = do
    tag_ <- getWord8
    case tag_ of
      255 -> return Unknown
      0 -> return Definition
      1 -> return Theorem
      2 -> return Notation
      3 -> return Tactic
      4 -> return Variable
      5 -> return Constructor
      6 -> return Proof
      7 -> return Library
      8 -> return Module
      9 -> return Section
      10 -> return Inductive
      11 -> return Axiom
      12 -> return Scheme
      13 -> return ModType
      14 -> return Instance
      15 -> return SynDef
      16 -> return Class
      17 -> return Record
      18 -> return Projection
      19 -> return Method
      _ -> fail "Binary_Kind_get: Kind cannot be parsed"
 --}

type Theory = Map.Map String Statement

instance Binary Code
instance Binary CoqKind

instance FromJSON Code where
  parseJSON (Object c) = CoqText <$> c .: "coqText"
  parseJSON invalid    = typeMismatch "Code" invalid

instance ToJSON Code where
 toJSON (CoqText c) = object [ "coqText" .= show c ]


instance FromJSON Kind where
 parseJSON (String "Type")     = return Type0
 parseJSON (String "Function") = return Function0
 parseJSON (String "Theorem")  = return Theorem0
 parseJSON invalid             = typeMismatch "Kind" invalid

instance ToJSON Kind where
 toJSON Type0     = toJSON ("Type"     :: String)
 toJSON Function0 = toJSON ("Function" :: String)
 toJSON Theorem0  = toJSON ("Theorem"  :: String)

fromCode :: Code -> Text
fromCode (CoqText t) = t 

data StatementA a = Statement { name :: Text
                              , kind :: CoqKind -- refactoring is needed
                              , code :: Code
                              , source:: String -- source filename isomorphism
                              , uses :: [a]
                           } deriving (Eq, Show, Generic)

type Statement = StatementA (Hash.Hash Hash.Id)

instance Binary Statement where
  put s = do
           put (name s)
           put (kind s)
           put (code s)
           put (source s)
           put (uses s)
  get = do n <- get
           k <- get
           c <- get
           sc <- get
           u <- get
           return $ Statement n k c sc u
