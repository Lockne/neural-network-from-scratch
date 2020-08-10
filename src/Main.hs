module Main where

import qualified Numeric.LinearAlgebra as LA

type Size = [Int]
type BiasSet = LA.Vector Double
type WeightSet = [LA.Matrix Double]

data Network =
  Network
    { numLayers :: Int
    , sizes :: [Int]
    , biases :: LA.Matrix Double
    , weights :: [LA.Matrix Double]
    }

instance Show (Network) where
  show (Network numLayers sizes biases weights) =
    concat
      [ "Network: "
      , "\n\tnumLayers: "
      , show numLayers
      , "\n\tsizes: "
      , show sizes
      , "\n\tbiases: "
      , show biases
      , "\n\tweights: "
      , show weights
      ]

initBias = undefined

initWeights = undefined

-- | Given the outputs of a previous layer, the biases of every node in the present layer,
--   and the weights for every node in the present layer, compute the z value for the present layer.
zLayer :: LA.Matrix Double -> LA.Matrix Double -> LA.Matrix Double -> LA.Matrix Double
zLayer input weights biases =  (weights <> input) + biases

sigmoid :: LA.Matrix Double -> LA.Matrix Double
sigmoid zlayer = 1 / (1 + LA.expm (-1*zlayer))

feedforward :: LA.Matrix Double -> LA.Matrix Double -> LA.Matrix Double -> LA.Matrix Double
feedforward = undefined



main :: IO ()
main = return ()
