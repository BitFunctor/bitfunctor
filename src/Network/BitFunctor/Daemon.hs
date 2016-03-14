module Network.BitFunctor.Daemon where

import Network.BitFunctor.Node as Node
import Network.BitFunctor.Forger as Forger
import Network.BitFunctor.P2P as P2P
import Network.BitFunctor.RestAPI as RestApi

main = putStrLn "Hello World"

run :: CurrencyDaemon
run = do
  core   <- currency Node.defaultSettings{ dbLocation = "~/.btf/blockchain" }  -- CurrencyMonad
  forger <- forkIO $ currencyForger core Forger.defaultSettings
  p2p    <- forkIO $ p2pNode core P2P.defaultSettings
  rest   <- forkIO $ restApi core RestApi.defaultSettings
