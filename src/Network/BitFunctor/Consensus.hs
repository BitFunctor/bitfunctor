module Network.BitFunctor.Consensus ( verify
                                    ) where

import Network.BitFunctor.Block.Types

import Data.Maybe
import Network.BitFunctor.Blockchain as B


-- | This one is needed only for consensus module
effectiveBalance :: LocalState -> BlockHash -> Account -> Int
effectiveBalance s b = B.accountBalanceAt s (HashOffset b -1440)



splitBlocks :: Int -> [Block]  -> ([Block], [Block])
splitBlocks k lb  | k < 0 = (lb, [])
splitBlocks k lb  | k >= 0 = splitAt k lb


calculateHit :: Block -> Account -> Integer
calculateHit prevBlock acc = fromIntegral $ first8bytesAsNumber $ sign (secKey acc) (pubKey acc) (encode prevBlock)


-- hitTime :: Account -> Block -> LocalView -> Timestamp
-- hitTime acct prevBl view = (blockTimestamp prevBl) + (calculateHit prevBl acct)*(effectiveBalance view acct) `div` (baseTarget prevBl)


verifyHit :: Integer -> Block -> Timestamp -> Int -> Bool
verifyHit hit prevBlock timestamp effBalance = (eta > 0) && hit < target -- && hit >= prevTarget) - after block 215000
    where eta = timestamp - blockTimestamp prevBlock
          effbt = (toInteger effBalance)*(baseTarget prevBlock)
          target = effbt*(toInteger eta)
          -- prevTarget = effbt * (eta-1)


verify :: Block -> Bool
verify = undefined  -- TODO: this is supposed to be refactored 'verifyHit'
