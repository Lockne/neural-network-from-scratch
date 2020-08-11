{-# LANGUAGE BangPatterns #-}

module Main where

-- | Some specifications as to the shapes of inputs, weights and biases:
--      1) The input to a neural net should be a 1 x c matrix.
--      2) For a particular layer z, the weight matrix for that layer should be a
--         (number of nodes in previous layer ) x (number of nodes in current layer ) matrix
--      3) For a particular layer z, the bias matrix for that layer should be a
--         (1) x (number of nodes in current layer) matrix.
import qualified Numeric.LinearAlgebra as LA
import Data.List

type Size = [Int]

data Network =
  Network
    { numLayers :: Int
    , sizes :: [Int]
    , biases :: [LA.Matrix Double]
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
zLayer ::
     LA.Matrix Double
  -> LA.Matrix Double
  -> LA.Matrix Double
  -> LA.Matrix Double
zLayer input weights biases = (input <> weights) + biases

-- | Having computed the z values to each node in the layer, the sigmoid function computes the
--   activation vector.

expm :: LA.Matrix Double -> LA.Matrix Double
expm = LA.fromLists . (fmap . fmap) exp . LA.toLists

sigmoid :: LA.Matrix Double -> (LA.Matrix Double, LA.Matrix Double)
sigmoid zlayer = (zlayer, 1 / (1 + expm (-1 * zlayer)))

-- -- | The feedforward algorithm is just data origami (fold left).
-- feedforward ::
--      LA.Matrix Double
--   -> [LA.Matrix Double]
--   -> [LA.Matrix Double]
--   -> LA.Matrix Double
-- feedforward inputs [] _ = inputs
-- feedforward inputs !(w:ws) !(b:bs) =
--   feedforward (snd $ sigmoid (zLayer inputs w b)) ws bs

-- | Outputs a list of (inputs, activations) for each layer as the input is forwarded
--   propagated through the network. This function is actually a scanl, but I have
--   to figure out how to write it in that way. Also the elements are listed in reverse
--   since I'll be backpropagating through this.

feedforward' ::
     LA.Matrix Double
  -> [LA.Matrix Double]
  -> [LA.Matrix Double]
  -> [(LA.Matrix Double, LA.Matrix Double)]
feedforward' inputs [] _ = []
feedforward' inputs (w:ws) (b:bs) =
  feedforward' (snd $ sigmoid (zLayer inputs w b)) ws bs ++ [sigmoid (zLayer inputs w b)]

-- | If y(x) is the expected output and a(x) is the network output for an input vector x,
--   then the cost function is given as:
--                              (1/2n) * SUM (a(x) - y(x))^2
cost :: LA.Matrix Double -> LA.Matrix Double -> LA.Matrix Double
cost networkOutput expectedOutput =
  (1 / 2 * (len expectedOutput)) * (networkOutput - expectedOutput) ^ 2
  where
    len = fromIntegral . (\(x, y) -> x * y) . LA.size


-- | Given input and expected output, calculate the total output error i.e.
--   the sum of errors of each node.
outputError ::
     LA.Matrix Double
  -> LA.Matrix Double
  -> [LA.Matrix Double]
  -> [LA.Matrix Double]
  -> Double
outputError input expOutput weights biases =
  LA.sumElements $ cost ((snd . head) $ feedforward' input weights biases) expOutput

-- | Time to make our backprop stew. But first, we'll need to chop the carrots and the potatoes.



backprop = undefined

test :: IO ()
test = do
  w1 <- LA.rand 2 3
  w2 <- LA.rand 3 1
  b1 <- LA.rand 1 3
  b2 <- LA.rand 1 1
  i <- LA.rand 1 2
  o <- LA.rand 1 2
  let b = [b1] ++ [b2]
  let w = [w1] ++ [w2]
  print $ feedforward' i w b
  print $  outputError i o w b




main :: IO ()
main = return ()
