module Network.BitFunctor2.Application.Registrar.Storage where

import Data.UUID
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

import Network.BitFunctor.Crypto.Key
import Network.BitFunctor.Crypto.Signing
import Network.BitFunctor2.Platform.Storage.Object
import Network.BitFunctor2.Platform.Blockchain.Types

data Data = Register   UUID PublicKey Text Signature
          | Unregister UUID Signature
  deriving (Eq, Show)


data Storage = Storage
  { storageObjects :: HashMap UUID (ObjectMeta, Object)
  } deriving (Eq, Show)

instance ObjectStorage Storage where
  -- put (m, o) s = Right $ Storage $ HM.insert (objectMetaId m) (m, o)
  --                                            (storageObjects s)
  put = undefined
  -- get uuid s   = (HM.lookup uuid (storageObjects s), s)
  get uuid s = undefined
