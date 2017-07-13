module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Network.BitFunctor.Account.Tests
import qualified Network.BitFunctor.Block.Tests
import qualified Network.BitFunctor.Transaction.Tests
import qualified Network.BitFunctor.Tests


main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ Network.BitFunctor.Account.Tests.tests
  , Network.BitFunctor.Block.Tests.tests
  , Network.BitFunctor.Transaction.Tests.tests
  , Network.BitFunctor.Tests.tests
  ]
