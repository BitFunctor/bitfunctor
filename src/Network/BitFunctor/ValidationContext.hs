{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.BitFunctor.ValidationContext where

import Network.BitFunctor.Transaction

import Control.Monad.Error
import Control.Monad.State


data BlockValidationContext = BlockValidationContext -- chain ledger

data TxValidationContext = TxValidationContext  -- ledger

-- TODO: should those contexts be instances of MonadError,
-- or just use ErrorT (or some flavor of MaybeT?) monad transformer?

-- computations of block/transaction application must be performed
-- in a corresponding context, which is some kind of combination
-- of a State monad with a ledger which can be changed and
-- ErrorT/Either monad for error-handling



data TxValidationError = InsufficientBalance
                       | InvalidSignature
                       | OtherTxValidationError String
                       deriving (Eq, Show)

data BlockValidationError = InvalidTx (TransactionHash, TxValidationError)
                          | InvalidSignature
                          | NoConsensus
                          | OtherBlockValidationError String
                          deriving (Eq, Show)


instance Error TxValidationError where
  strMsg = OtherTxValidationError

instance Error BlockValidationError where
  strMsg = OtherBlockValidationError


-- inspired by http://book.realworldhaskell.org/read/error-handling.html

-- should have one more nesting level for Tx application/TxValidationError

newtype StateModifier = SM {
  runSM :: ErrorT BlockValidationError (State Ledger) Ledger
} deriving (Monad, MonadError BlockValidationError)


liftSM :: State Ledger -> StateModifier
liftSM m = SM (lift m)


runStateModifier :: StateModifier -> Ledger
                 -> Either BlockValidationError Ledger
runStateModifier sm le = case runState (runErrorT (runSM sm)) le of
                           (Left e,   _) -> Left e
                           (Right _, le) -> Right le

modifyWithBlock :: Block -> StateModifier
modifyWithBlock = undefined

runStateModifier (modifyWithBlock b)
