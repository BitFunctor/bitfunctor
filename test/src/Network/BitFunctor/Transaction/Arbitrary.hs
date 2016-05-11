module Network.BitFunctor.Transaction.Arbitrary where

import Network.BitFunctor.Account
import Network.BitFunctor.Account.Arbitrary()
import Network.BitFunctor.Asset
import Network.BitFunctor.Transaction

import Data.Maybe (fromJust)

import Test.QuickCheck
import Test.QuickCheck.Instances()


instance Arbitrary TxInputType where
  arbitrary = do
    btfValue <- arbitrary
    return $ Value $ fromInteger btfValue

instance Arbitrary Transaction where
  arbitrary = do
    accFrom <- arbitrary
    accIdTo <- arbitrary

    -- TxInput
    let frm = toAccountId accFrom
    what <- arbitrary   -- TxInputType
    let i = TxInput frm what

    let o = TxOutput accIdTo
    let f = 1
    t <- arbitrary

    let tx = Transaction i o f t undefined
    let signedTx = fromJust $ sign accFrom tx
    return signedTx
