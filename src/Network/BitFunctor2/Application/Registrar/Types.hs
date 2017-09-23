module Network.BitFunctor2.Application.Registrar.Types where

import Data.UUID
import Data.Text (Text)

import Network.BitFunctor.Crypto.Types


data ObjectTypeRegistrarData = ObjectTypeRegister   UUID PublicKey Text Signature
                             | ObjectTypeUnregister UUID Signature
  deriving (Eq, Show)
