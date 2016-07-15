{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies, TemplateHaskell, TypeSynonymInstances, FlexibleInstances, DeriveGeneric, StandaloneDeriving #-}

module Network.BitFunctor.Theory.Coq.TheoryAcid where

import GHC.Generics
import Data.Acid
import Data.Acid.Remote
import Data.Typeable
import Control.Monad.State
import Control.Monad.Reader
import Control.Applicative
import Data.SafeCopy
import Data.Text
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Serialize as DSer


import Network.BitFunctor.Theory.Types
import Network.BitFunctor.Theory.Coq.Types
import qualified Network.BitFunctor.Theory.Extraction as TE (extractTermsCode, getStatementsBySuffix)

data CoqTheoryAcid = CoqTheoryAcid CoqTheoryT
                     deriving Typeable

data AcidResult a = AcidResultSuccess a | AcidResultError
                    deriving (Generic, Typeable, Show)

instance DSer.Serialize a => DSer.Serialize (AcidResult a)
instance DSer.Serialize a => SafeCopy (AcidResult a)

instance FromJSON a => FromJSON (AcidResult a)
instance ToJSON a => ToJSON (AcidResult a) where
   toEncoding = genericToEncoding defaultOptions

$(deriveSafeCopy 0 'base ''CoqTheoryAcid)

insertKey :: CoqStatementName -> CoqStatementT -> Update CoqTheoryAcid (AcidResult ())
insertKey n s = do
                  CoqTheoryAcid m <- get
                  put $ CoqTheoryAcid $ insertStatementKey n s m
                  return $ AcidResultSuccess ()

lookupKey :: CoqStatementName -> Query CoqTheoryAcid (AcidResult CoqStatementT)
lookupKey n = do
                CoqTheoryAcid m <- ask
                let mv = lookupStatementKey n m
                case mv of
                 Just v -> return $ AcidResultSuccess v
                 Nothing -> return $ AcidResultError

thSize :: Query CoqTheoryAcid (AcidResult Int)
thSize = do
           CoqTheoryAcid m <- ask
           return $ AcidResultSuccess $ theorySize m

initTheoryFromList :: [CoqStatementT] -> Update CoqTheoryAcid (AcidResult ())
initTheoryFromList tl = do
                         put $ CoqTheoryAcid $ fromStatementList tl
                         return $ AcidResultSuccess ()

extractTerm :: Text -> Query CoqTheoryAcid (AcidResult [Text])
extractTerm term = do                     
                     CoqTheoryAcid th <- ask 
                     let ecl' = TE.extractTermsCode th [term]
                     let ecl = Prelude.head ecl'                     
                     case ecl of
                       [] -> return $ AcidResultError
                       l -> return $ AcidResultSuccess l

getStatement :: Text -> Query CoqTheoryAcid (AcidResult [CoqStatementT])
getStatement stn = do
                     CoqTheoryAcid th <- ask 
                     let sts = TE.getStatementsBySuffix th stn
                     case sts of
                       [] -> return $ AcidResultError
                       l -> return $ AcidResultSuccess l                     

$(makeAcidic ''CoqTheoryAcid ['insertKey, 'lookupKey, 'thSize, 'initTheoryFromList, 'extractTerm, 'getStatement])

deriving instance Show InsertKey
deriving instance Show LookupKey
deriving instance Show ThSize
deriving instance Show InitTheoryFromList
deriving instance Show ExtractTerm
deriving instance Show GetStatement

deriving instance Generic InsertKey
deriving instance Generic LookupKey
deriving instance Generic ThSize
deriving instance Generic InitTheoryFromList
deriving instance Generic ExtractTerm
deriving instance Generic GetStatement


instance FromJSON InsertKey
instance ToJSON InsertKey where
   toJSON = genericToJSON defaultOptions

instance FromJSON LookupKey
instance ToJSON LookupKey where
   toJSON = genericToJSON defaultOptions

instance FromJSON ThSize
instance ToJSON ThSize where
   toJSON = genericToJSON defaultOptions

instance FromJSON InitTheoryFromList
instance ToJSON InitTheoryFromList where
   toJSON = genericToJSON defaultOptions

instance FromJSON ExtractTerm
instance ToJSON ExtractTerm where
   toJSON = genericToJSON defaultOptions

instance FromJSON GetStatement
instance ToJSON GetStatement where
   toEncoding = genericToEncoding defaultOptions


data CoqTheoryAcidQuery = InsertKeyC InsertKey |
                          LookupKeyC LookupKey |
                          ThSizeC ThSize |
                          InitTheoryFromListC InitTheoryFromList |
                          ExtractTermC ExtractTerm |
                          GetStatementC GetStatement 
                          deriving (Show, Generic)

instance FromJSON CoqTheoryAcidQuery
instance ToJSON CoqTheoryAcidQuery where
   toJSON = genericToJSON defaultOptions
