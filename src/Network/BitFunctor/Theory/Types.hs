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
import Data.Text as DT
import Data.Binary
import qualified Network.BitFunctor.Crypto.Hash as Hash

data Code = CoqText Text
            deriving (Eq, Show, Generic)

-- 20+1 constructors
data CoqKind = Unknown | Definition | Theorem | Notation | Tactic | Variable | Constructor | Proof |
               Library | Module | Section | Inductive | Axiom | Scheme | ModType | Instance | SynDef |
               Class | Record | Projection | Method
            deriving (Eq, Ord, Show, Generic)

data ResourceKind = Resource | StopStatement | IgnorableRes
                    deriving (Eq, Show)

-- obsolete type, refactoring is needed
data Kind = Type0 | Function0 | Theorem0
            deriving (Eq, Show, Generic)

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

data CoqStatementName = CoqStatementName { libname :: Text
                                         , modname :: Text
                                         , sname :: Text} deriving (Eq, Ord, Show, Generic)

fqStatementName s = DT.append (if (libname s == "") then "" else DT.append (libname s) ".")
                    (DT.append (if (modname s == "") then "" else DT.append (modname s) ".") (sname s))  

data StatementA a = Statement { stname :: CoqStatementName
                              , stkind :: CoqKind -- refactoring is needed
                              , stcode :: Code
                              , stsource:: Hash.Hash Hash.Id -- source filename isomorphism
                              , stuses :: [a]
                           } deriving (Eq, Show, Generic)

type Statement = StatementA (Hash.Hash Hash.Id)

instance Binary CoqStatementName

instance Binary Statement
