{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Numeric.LinearAlgebra
import Control.Monad
import Control.Monad.Random
import Control.Applicative (liftA2)



data Weights = W { wBiases :: !(Vector Double) -- n
                 , wNodes :: !(Matrix Double)  -- n x m
                 }                             -- "m to n" layer

data Network :: * where
  O :: !Weights -> Network
  (:~) :: !Weights -> !Network -> Network

infixr 5 :~

-- | Generate random weights.

randomWeights :: MonadRandom m => Int -> Int -> m Weights
randomWeights i o = do
  seed1  :: Int <- getRandom
  seed2  :: Int <- getRandom
  let wB = randomVector seed1 Uniform o
      wN = uniformSample seed2 o (replicate i (-1, 1))
  return $ W wB wN

-- | Generate a random Network
randomNet :: MonadRandom m => Int -> [Int] -> Int -> m Network
randomNet i [] o = O <$> randomWeights i o
randomNet i (h:hs) o = liftA2 (:~) (randomWeights i h) (randomNet i hs o)

-- | Relu function
relu :: (Floating a, Ord a) => a -> a
relu = max 0

relu' :: (Floating a, Ord a) => a -> a
relu' x | x < 0 = 0
        | otherwise = 1

train :: Double -> Vector Double -> Vector Double -> Network -> Network
train = undefined
