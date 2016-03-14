module Network.BitFunctor.Consensus.Difficulty where


import Blockchain (block)

initialBaseTarget :: Integer
initialBaseTarget = div maxHit (2*goalBlockTime*(fromIntegral systemBalance))

maxBaseTarget :: Integer
maxBaseTarget = initialBaseTarget * (fromIntegral systemBalance)

--correct this!
difficultyFunction :: Integer -> Double
difficultyFunction x = 20e8/(fromIntegral x)


cumulativeDifficulty :: BlockChain -> Double
cumulativeDifficulty chain = foldl (\cd b -> cd + (difficultyFunction $ baseTarget b)) 0 chain

cumulativeNodeDifficulty :: Node -> Double
cumulativeNodeDifficulty node = totalDifficulty $ bestBlock $ localView node

nextBaseTarget :: LocalState -> UTCTime -> Integer
nextBaseTarget state time = min (max mint candidate) maxt
                     where
                       mint = max (currentt `div` 2) 1
                       maxt = min (2 * currentt) maxBaseTarget
                       currentt  = baseTarget $ block state Head
                       candidate = currentt * timespan `div` goalBlockTime
                       timespan  = toInteger (time - (timestamp $ block state Head))
