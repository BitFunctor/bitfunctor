module Network.BitFunctor.Token where

import Network.BitFunctor.Crypto.Hash
import Data.Binary

-- | Any type can be used as a Token if it can provide a value in units
class Token t where
  value :: t -> Integer


-- | BTF – underlying token type.
-- Contains value in units (identity token container, actually)
newtype BTF = BTF Integer
              deriving (Show, Eq, Ord)

instance Token BTF where
  value (BTF v) = v

instance Binary BTF where
  put (BTF v) = put v
  get = do
    v <- get
    return $ BTF v

-- Num typeclass defines too much things besides (+) and (-)...
(+.) :: BTF -> BTF -> BTF
(BTF v) +. (BTF u) = BTF (u + v)

(-.) :: BTF -> BTF -> BTF
(BTF v) -. (BTF u) = BTF (u - v)


-- | cBTF – contribution BTF token granted for contribution to the BTF theory.
-- Each one is unique (depends on tx id).
-- Value to be determined using complexity theory state at a time.
data CBTF = CBTF (Hash Id)
            deriving (Show, Eq)

instance Token CBTF where
  value (CBTF originTx) = undefined -- TODO: get value using current complexity theory state

instance Binary CBTF where
  put (CBTF h) = put h
  get = do
    h <- get
    return $ CBTF h
