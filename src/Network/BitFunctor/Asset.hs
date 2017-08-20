module Network.BitFunctor.Asset ( Asset (..)
                                , BTF
                                , CBTF (..)
                                ) where

import Network.BitFunctor.Crypto.Hash
import Data.Binary


-- | Any type can be used as an asset if it can provide its quantity/measure
class Asset a where
  measure  :: a -> Integer
  quantity :: a -> Integer

  measure = quantity


-- | BTF – underlying asset type.
-- All other assets' valuations are in BTFs.
-- BTFs are valued trivially as its own quantity/measure
newtype BTF = BTF { units :: Integer }
                  deriving (Eq, Ord, Show)

-- | cBTF – contribution BTF asset granted for contribution to the BTF theory.
-- Each one is unique (depends on tx id).
-- Value to be determined using theory complexity theory state at a time of such request.
-- Can be exchanged (burned) to BTF at the rate equal value (or less) at the time of the exchange.
newtype CBTF c = CBTF { contributionId :: c }
                    deriving (Eq, Ord, Show)


instance Asset BTF where
  quantity = units

instance Num BTF where
  a + b = BTF $ units a + units b
  a - b | a >= b    = BTF $ units a - units b
        | otherwise = error "num: not enough assets"
  _ * _    = error "num: can't multiply assets – makes no sense"
  negate   = error "num: can't negate asset quantity"
  abs      = BTF . abs . units
  signum _ = BTF 1
  fromInteger x = BTF { units = x }

instance Binary BTF where
  put btf = do
    put (0 :: Word8)
    put $ units btf
  get = do
    tag <- get
    case tag :: Word8 of
      0 -> do
        us <- get
        return BTF { units = us }
      _ -> fail "binary: can't parse btf asset (wrong tag)"


instance Asset (CBTF c) where
  quantity _ = 1

instance Binary c => Binary (CBTF c) where
  put cbtf = do
    put (1 :: Word8)
    put $ contributionId cbtf
  get = do
    tag <- get
    case tag :: Word8 of
      1 -> do
        cid <- get
        return CBTF { contributionId = cid }
      _ -> fail "binary: can't parse cbtf asset (wrong tag)"
