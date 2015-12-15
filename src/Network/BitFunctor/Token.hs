module Network.BitFunctor.Token where

import Network.BitFunctor.Crypto.Hash


-- | Any type can be used as a Token if it can provide a value in units
class Token t where
  value :: t -> Integer


-- | BTC – underlying token type. 
-- Contains value in units (identity token container, actually)
newtype BTF = BTF Integer
              deriving (Show, Eq)

instance Token BTF where
  value (BTF v) = v


-- | cBTF – contribution BTF token granted for contribution to the BTF theory.
-- Each one is unique (depends on tx id).
-- Value to be determined using complexity theory state at a time.
data CBTF = CBTF (Hash Id)
            deriving (Show, Eq)

instance Token CBTF where
  value (CBTF originTx) = undefined -- TODO: get value using current complexity theory state
