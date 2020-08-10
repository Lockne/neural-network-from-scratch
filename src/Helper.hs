type Bias = Float

type Inputs = [Float]

type Weights = [Float]

-- | Function that takes a dot product of two lists
dot :: Inputs -> Weights -> Float
dot xs ys = sum $ zipWith (*) xs ys

-- | Implement a sigmoid neuron.
sigmoid :: Inputs -> Weights -> Bias -> Float
sigmoid inputs weights bias = 1 / (1 + exp (-z))
  where
    z = dot inputs weights + bias
