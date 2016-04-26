{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.BitFunctor.Theory.Coq.Types where

import qualified Data.Map as Map
import Data.ByteString
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import Control.Applicative
import Data.Text as Text
import Data.Monoid
import Data.Either (rights)
import qualified Data.List as List
import qualified Data.Text.Encoding as TE
import Data.Binary
import qualified Network.BitFunctor.Crypto.Hash as Hash
import qualified Data.Serialize as DS
import qualified Data.ByteString as DBS
import qualified Data.ByteString.Base16 as B16 (encode, decode)
import qualified Data.ByteArray as DBA (convert)
import Control.Lens

import qualified Network.BitFunctor.Common as Common
import Network.BitFunctor.Theory.Types

data CoqStatementName = CoqStatementName { _libname :: Text
                                         , _modname :: Text
                                         , _sname :: Text} deriving (Eq, Ord, Show, Generic)
makeLenses ''CoqStatementName

data CoqKind = Unknown | Definition | Theorem | Notation | Tactic | Variable | Constructor | Proof |
               Library | Module | Section | Inductive | Axiom | Scheme | ModType | Instance | SynDef |
               Class | Record | Projection | Method | SelfReference | BoundVariable | LocalConstructor
               deriving (Eq, Ord, Show, Generic)

type CoqTerm = (CoqKind, CoqStatementName)

type CoqCode = CodeA Text CoqTerm

instance Keyable CoqStatementName CoqStatementName where
   toKey = id

instance Nameable CoqStatementName CoqStatementName where
   toPrefix c csn = let l = csn^.libname in
                    let m = csn^.modname in
                    if Text.null l then m
                    else if (Text.null m) then l
                                          else l <> (Text.singleton c) <> m
   toSuffix = _sname
   fromSuffix t = CoqStatementName "" "" t
   fromKey k _ = k

instance Codeable Text where
   toText = id
   fromText = id
   isFQExtractable _ = False
   isTheoriable _ = False
   isSelfReference _ = False

instance Keyable CoqTerm CoqStatementName where
   toKey = snd 

instance Nameable CoqTerm CoqStatementName where
   toPrefix c = toPrefix c . snd
   toSuffix = toSuffix . snd
   fromSuffix t = (Unknown, fromSuffix t)
   fromKey k x = (fst x, fromKey k (snd x)) 

instance Codeable CoqTerm where
   -- check whether it is correct
   toText = toSuffix . snd
   fromText t = (Unknown, fromSuffix t)
   isSelfReference ct = fst ct == SelfReference
   isFQExtractable ct = fst ct /= BoundVariable
   isTheoriable ct = fst ct /= BoundVariable && fst ct /= SelfReference && fst ct /= LocalConstructor

-- (Text -> Text -> Bool) -> Char -> a -> a -> a
-- changeSelfReference t' t u  = (fst u, changeNameWith (==) (snd t') (snd t) u)

data ResourceKind = Resource | StopStatement | IgnorableRes
                    deriving (Eq, Show)

data CoqStatementA a = CoqStatementA { _stname :: CoqStatementName
                                     , _stkind :: CoqKind                                   
                                     , _stcode :: CoqCode
                                     , _stsource :: Hash.Hash Hash.Id
                                     , _stuses :: [a]
                                     } deriving (Eq, Show, Generic)

makeLenses ''CoqStatementA

instance DS.Serialize CoqKind
instance DS.Serialize UsesBottom
instance DS.Serialize CoqStatementName
instance Binary CoqStatementName
instance DS.Serialize a => DS.Serialize (CoqStatementA a)

instance (Eq a, DS.Serialize a) => StatementC CoqStatementName CoqStatementName Text CoqTerm (CoqStatementA a) where
    toStatementName = _stname
    toStatementCode = _stcode
    changeStatementCode c s = stcode .~ c $ s
    toStatementKey = _stname


type PreCoqTheory a = Map.Map CoqStatementName (CoqStatementA a) 

data UsesBottom  = Bottom deriving (Eq, Show, Generic)
type CoqStatementT = CoqStatementA UsesBottom
type CoqTheoryT = PreCoqTheory CoqStatementT


instance (Eq a, DS.Serialize a) => TheoryC CoqStatementName CoqStatementName Text CoqTerm (CoqStatementA a) (PreCoqTheory a) where    
    toStatementList t = Map.elems t
    toStatementMap t = t

instance (Eq a, DS.Serialize a) => TheoryC CoqStatementName CoqStatementName Text CoqTerm (CoqStatementA a) [CoqStatementA a] where    
    toStatementList t = t
    

-- obsolete type, refactoring is needed
data Kind = Type0 | Function0 | Theorem0
            deriving (Eq, Show, Generic)


-- instance DS.Serialize CoqTheoryT


instance PartOrd (CoqStatementA a) where
   partCompare s1 s2 = let sc1 = s1^.stcode in
                       let sc2 = s2^.stcode in
                       case (sc1, sc2) of
                         (Left _, _) -> Common.PNC
                         (_, Left _) -> Common.PNC
                         (Right cl1, Right cl2) ->
                                     if (List.elem (s1^.stname) $ List.map snd $ rights cl2) then Common.PLT else
                                     if (List.elem (s2^.stname) $ List.map snd $ rights cl1) then Common.PGT else
                                     if (s1^.stname == s2^.stname) then Common.PEQ else
                                     Common.PNC
{--
ordStatement s1 s2 = if (Map.member (stname s1) $ Map.fromList (List.map (\u -> (snd u, True)) $ stuses s2)) then Common.PLT else
                     if (Map.member (stname s2) $ Map.fromList (List.map (\u -> (snd u, True)) $ stuses s1)) then Common.PGT else
                     if (stname s1 == stname s2) then Common.PEQ else
                     Common.PNC--}

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


{--

type HashId = Hash.Hash Hash.Id
type Statement = StatementA HashId
type Theory = Map.Map String Statement
type HashCodePart = CoqCodePart HashId
type HashCode = Code HashId


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
--}

-- instance DS.Serialize PreTheoryList
