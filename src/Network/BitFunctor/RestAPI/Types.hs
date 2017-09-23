module Network.BitFunctor.RestAPI.Types where

type BitFunctorAPI = BlockchainAPI :<|> TheoryAPI :<|> WalletAPI
