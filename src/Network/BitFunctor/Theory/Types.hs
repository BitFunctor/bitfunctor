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
import qualified Data.Text.Encoding as TE
import Data.Binary
import qualified Network.BitFunctor.Crypto.Hash as Hash
import qualified Data.Serialize as DS
import qualified Data.ByteString as DBS
import qualified Data.ByteString.Base16 as B16 (encode, decode)
import qualified Data.ByteArray as DBA (convert)


data CoqCodePart a = CoqCodeText Text | CoqCodeEntry a deriving (Eq, Show, Generic)

data Code a = CoqText Text | CoqCodeParts [CoqCodePart a]
              deriving (Eq, Show, Generic)

-- 20+1 constructors
data CoqKind = Unknown | Definition | Theorem | Notation | Tactic | Variable | Constructor | Proof |
               Library | Module | Section | Inductive | Axiom | Scheme | ModType | Instance | SynDef |
               Class | Record | Projection | Method | SelfReference | BoundVariable | LocalConstructor
            deriving (Eq, Ord, Show, Generic)

data ResourceKind = Resource | StopStatement | IgnorableRes
                    deriving (Eq, Show)

-- obsolete type, refactoring is needed
data Kind = Type0 | Function0 | Theorem0
            deriving (Eq, Show, Generic)

--instance FromJSON Code where
--  parseJSON (Object c) = CoqText <$> c .: "coqText"
--  parseJSON invalid    = typeMismatch "Code" invalid

--instance ToJSON Code where
-- toJSON (CoqText c) = object [ "coqText" .= show c ]


--instance FromJSON Kind where
-- parseJSON (String "Type")     = return Type0
-- parseJSON (String "Function") = return Function0
-- parseJSON (String "Theorem")  = return Theorem0
-- parseJSON invalid             = typeMismatch "Kind" invalid

--instance ToJSON Kind where
-- toJSON Type0     = toJSON ("Type"     :: String)
-- toJSON Function0 = toJSON ("Function" :: String)
-- toJSON Theorem0  = toJSON ("Theorem"  :: String)

-- fromCode :: Code a -> Text
-- fromCode (CoqText t) = t

data CoqStatementName = CoqStatementName { libname :: Text
                                         , modname :: Text
                                         , sname :: Text} deriving (Eq, Ord, Show, Generic)

fqStatementName s = DT.append (if (libname s == "") then "" else DT.append (libname s) ".")
                    (DT.append (if (modname s == "") then "" else DT.append (modname s) ".") (sname s))  

data StatementA a = Statement { stname :: CoqStatementName
                              -- , origstname :: CoqStatementName
                              , stkind :: CoqKind -- refactoring is needed
                              , stcode :: Code a
                              , stsource:: Hash.Hash Hash.Id -- source filename isomorphism
                              , stuses :: [a]
                           } deriving (Eq, Show, Generic)

type HashId = Hash.Hash Hash.Id
type Statement = StatementA HashId
type Theory = Map.Map String Statement
type HashCodePart = CoqCodePart HashId
type HashCode = Code HashId

type CoqTerm = (CoqKind, CoqStatementName)
type PreTheory a = Map.Map CoqStatementName (StatementA a)
type PreStatementWithList = StatementA CoqTerm
type PreTheoryWithList = PreTheory CoqTerm
type PreCode = Code CoqTerm
type PreCodePart = CoqCodePart CoqTerm
type PreTheoryList = [PreStatementWithList]


instance Binary HashCodePart
instance Binary HashCode
instance Binary CoqKind
instance Binary CoqStatementName
instance Binary Statement

instance DS.Serialize DT.Text where
  put txt = DS.put $ TE.encodeUtf8 txt
  get     = fmap TE.decodeUtf8 DS.get

instance DS.Serialize CoqStatementName
instance DS.Serialize CoqKind
instance DS.Serialize PreCode
instance DS.Serialize PreCodePart
instance DS.Serialize PreStatementWithList
-- instance DS.Serialize PreTheoryList
