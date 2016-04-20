module Network.BitFunctor.Transaction.Arbitrary where

import Network.BitFunctor.Account
import Network.BitFunctor.Account.Arbitrary()
import Network.BitFunctor.Token
import Network.BitFunctor.Transaction

import Data.Maybe (fromJust)

import Test.QuickCheck
import Test.QuickCheck.Instances()


instance Arbitrary TxInputType where
  arbitrary = do
    btfValue <- arbitrary
    return $ Value $ BTF btfValue

instance Arbitrary Transaction where
  arbitrary = do
    accFrom <- arbitrary
    accIdTo <- arbitrary

    let frm = toAccountId accFrom
    what <- arbitrary
    let i = TxInput frm what
    let o = TxOutput accIdTo
    let f = BTF 1
    t <- arbitrary
    let tx = Transaction i o f t undefined
    return . fromJust $ sign accFrom tx
