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


import Network.BitFunctor.Theory.Types
import Network.BitFunctor.Theory.Coq.Types
import qualified Network.BitFunctor.Theory.Extraction as TE (extractTermsCode)

data CoqTheoryAcid = CoqTheoryAcid CoqTheoryT
                     deriving Typeable

$(deriveSafeCopy 0 'base ''CoqTheoryAcid)

insertKey :: CoqStatementName -> CoqStatementT -> Update CoqTheoryAcid ()
insertKey n s = do
                  CoqTheoryAcid m <- get
                  put $ CoqTheoryAcid $ insertStatementKey n s m

lookupKey :: CoqStatementName -> Query CoqTheoryAcid (Maybe CoqStatementT)
lookupKey n = do
                CoqTheoryAcid m <- ask
                return $ lookupStatementKey n m

thSize :: Query CoqTheoryAcid Int
thSize = do
           CoqTheoryAcid m <- ask
           return $ theorySize m

initTheoryFromList :: [CoqStatementT] -> Update CoqTheoryAcid ()
initTheoryFromList tl = put $ CoqTheoryAcid $ fromStatementList tl

extractTerm :: Text -> Query CoqTheoryAcid [Text]
extractTerm term = do                     
                     CoqTheoryAcid th <- ask 
                     let ecl' = TE.extractTermsCode [term] th
                     let ecl = Prelude.concat ecl'                     
                     return ecl                     

$(makeAcidic ''CoqTheoryAcid ['insertKey, 'lookupKey, 'thSize, 'initTheoryFromList, 'extractTerm])

deriving instance Show InsertKey
deriving instance Show LookupKey
deriving instance Show ThSize
deriving instance Show InitTheoryFromList
deriving instance Show ExtractTerm

deriving instance Generic InsertKey
deriving instance Generic LookupKey
deriving instance Generic ThSize
deriving instance Generic InitTheoryFromList
deriving instance Generic ExtractTerm

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


data CoqTheoryAcidQuery = InsertKeyC InsertKey |
                          LookupKeyC LookupKey |
                          ThSizeC ThSize |
                          InitTheoryFromListC InitTheoryFromList |
                          ExtractTermC ExtractTerm
                          deriving (Show, Generic)

instance FromJSON CoqTheoryAcidQuery
instance ToJSON CoqTheoryAcidQuery where
   toJSON = genericToJSON defaultOptions


--   toJSON (InsertKeyC (InsertKey k v))  = object ["query" .= "InsertKey", "key" .= toJSON k, "value" .= toJSON v]
