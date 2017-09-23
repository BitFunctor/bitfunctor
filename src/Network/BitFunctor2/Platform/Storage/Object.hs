module Network.BitFunctor2.Platform.Storage.Object where

import Data.UUID

import Network.BitFunctor2.Platform.Blockchain.Types


class ObjectStorage s where
  put :: (ObjectMeta, o) -> s -> Either e s
  get :: UUID -> s -> (Maybe o, s)


data ObjectStorageError = ObjectStorageError

