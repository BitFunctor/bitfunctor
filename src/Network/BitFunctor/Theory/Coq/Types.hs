{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}


module Network.BitFunctor.Theory.Coq.Types where

import qualified Data.Map.Strict as Map
import Data.ByteString
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (typeMismatch, defaultOptions)
import Control.Applicative
import Data.Text as Text
import Data.Monoid
import Data.Either (rights)
import qualified Data.List as List
import qualified Data.Text.Encoding as TE
import Data.Binary
import qualified Network.BitFunctor.Crypto.Hash as Hash
import qualified Data.ByteString as DBS
import qualified Data.ByteString.Base16 as B16 (encode, decode)
import qualified Data.ByteArray as DBA (convert)
import Data.Typeable (Typeable (..))
import Data.SafeCopy (SafeCopy (..))
import qualified Data.Serialize as DSer (Serialize (..))

import Control.Lens

import qualified Network.BitFunctor.Common as Common
import Network.BitFunctor.Theory.Types

newtype NewText = NewText Text.Text

instance Binary NewText where
  put (NewText txt) = put $ TE.encodeUtf8 txt
  get     =  fmap TE.decodeUtf8 get >>= \a -> return $ NewText a

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
   fromKey k _ = k

instance Nameable CoqStatementName CoqStatementName where
   toPrefix c csn = let l = csn^.libname in
                    let m = csn^.modname in
                    if Text.null l then m
                    else if (Text.null m) then l
                                          else l <> (Text.singleton c) <> m
   toSuffix = _sname
   fromSuffix t = CoqStatementName "" "" t
   changePrefix p (CoqStatementName _ _ s) = CoqStatementName p "" s

instance Codeable Text where
   toText = id
   fromText = id
   isFQExtractable _ = False
   isTheoriable _ = False
   isSelfReference _ = False

instance Keyable CoqTerm CoqStatementName where
   toKey = snd
   fromKey k x = (fst x, fromKey k (snd x))  

instance Nameable CoqTerm CoqStatementName where
   toPrefix c = toPrefix c . snd
   toSuffix = toSuffix . snd
   fromSuffix t = (Unknown, fromSuffix t)
   changePrefix p x = (fst x, changePrefix p $ snd x)

instance Codeable CoqTerm where
   -- check whether it is correct
   toText = toSuffix . snd
   fromText t = (Unknown, fromSuffix t)
   isSelfReference ct = fst ct == SelfReference
   isFQExtractable ct = fst ct /= BoundVariable
   isTheoriable ct = fst ct /= BoundVariable && fst ct /= SelfReference && fst ct /= LocalConstructor


data ResourceKind = Resource | StopStatement | IgnorableRes
                    deriving (Eq, Show)

data CoqStatementA a = CoqStatementA { _stname :: CoqStatementName
                                     , _stkind :: CoqKind                                   
                                     , _stcode :: CoqCode
                                     , _stsource :: Hash.Hash Hash.Id
                                     , _stuses :: [a]
                                     } deriving (Eq, Show, Generic, Typeable)

makeLenses ''CoqStatementA

instance DSer.Serialize Text.Text where
  put txt = DSer.put $ TE.encodeUtf8 txt
  get     = fmap TE.decodeUtf8 DSer.get

instance Binary CoqKind
instance DSer.Serialize CoqKind
instance SafeCopy CoqKind

instance Binary UsesBottom
instance DSer.Serialize UsesBottom
instance SafeCopy UsesBottom

instance Binary CoqStatementName
instance DSer.Serialize CoqStatementName
instance SafeCopy CoqStatementName

instance FromJSON CoqStatementName
instance ToJSON CoqStatementName where
   toJSON = genericToJSON defaultOptions

instance FromJSON UsesBottom
instance ToJSON UsesBottom where
   toJSON = genericToJSON defaultOptions

instance FromJSON CoqKind
instance ToJSON CoqKind where
   toJSON = genericToJSON defaultOptions


instance FromJSON a => FromJSON (CoqStatementA a)
instance ToJSON a => ToJSON (CoqStatementA a)
instance Binary a => Binary (CoqStatementA a)
instance DSer.Serialize a => DSer.Serialize (CoqStatementA a)
instance DSer.Serialize a => SafeCopy (CoqStatementA a)

instance (Eq a, Binary a) => StatementC CoqStatementName CoqStatementName Text CoqTerm (CoqStatementA a) where
    toStatementName = _stname
    toStatementCode = _stcode
    changeStatementCode c s = stcode .~ c $ s
    toStatementKey = _stname
    changeStatementName n s = stname .~ n $ s


type PreCoqTheory a = Map.Map CoqStatementName (CoqStatementA a)

data UsesBottom  = Bottom deriving (Eq, Show, Generic, Typeable)
type CoqStatementT = CoqStatementA UsesBottom
type CoqTheoryT = PreCoqTheory UsesBottom


instance (Eq a, Binary a) => TheoryC CoqStatementName CoqStatementName Text CoqTerm (CoqStatementA a) (PreCoqTheory a) where    
    toStatementList = Map.elems
    toStatementMap = id
    fromStatementMap = id
    fromStatementList = toStatementMap


instance (Eq a, Binary a) => TheoryC CoqStatementName CoqStatementName Text CoqTerm (CoqStatementA a) [CoqStatementA a] where    
    toStatementList = id
    fromStatementMap = Map.elems
    fromStatementList = id


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




-- obsolete type, refactoring is needed
data Kind = Type0 | Function0 | Theorem0
            deriving (Eq, Show, Generic)


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
