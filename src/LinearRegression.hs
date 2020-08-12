import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Data as D

type X_Train = LA.Matrix Double
type Y_Train = LA.Matrix Double
type W = LA.Matrix Double
type B = LA.Matrix Double
type Output = LA.Matrix Double
type LearningRate = Double
-- | y = w * x + b for a single training example

-- | In matrix notation,  [y] = [w]T . [x] + [b]

output :: W -> X_Train -> B -> Output
output w xTrain b = (D.tr (w) <> xTrain) + b

cost :: Output -> Y_Train -> Double
cost output yTrain = LA.sumElements $ (1/2) * (output - yTrain) ^ 2

deltaCW :: W -> X_Train -> Y_Train -> B -> LA.Matrix Double
deltaCW w xTrain yTrain b = ((D.tr (w) <> xTrain) + b) <> xTrain

-- | I think this is quite wrong. Hahaha
deltaCB :: W -> X_Train -> Y_Train -> B -> LA.Matrix Double
deltaCB w xTrain yTrain b = (D.tr (w) <> xTrain) + b

updateW :: W -> X_Train -> Y_Train -> B -> LearningRate -> W
updateW w xTrain yTrain b alpha = w - alpha `LA.scale` deltaCW w xTrain yTrain b

updateB :: W -> X_Train -> Y_Train -> B -> LearningRate -> W
updateB w xTrain yTrain b alpha = b - alpha `LA.scale` deltaCB w xTrain yTrain b

main = do
  xTrain <- LA.rand 3 1
  yTrain <- LA.rand 3 1
  w <- LA.rand 3 1
  b <- LA.rand 1 1
  let alpha = 0.0001
  let loop i w' b' | i > 1000 = return ()
      loop i w' b' | i <= 1000 = do
         let c = cost (output w' xTrain b') yTrain
         putStrLn $ concat ["Cost: ", show c]
  --       putStrLn "\n"
         let w'' = updateW w' xTrain yTrain b' alpha
         let b'' = updateB w' xTrain yTrain b' alpha
--         putStrLn $ concat ["Weights: ", show w'']
--         putStrLn "\n"
--         putStrLn $ concat ["Bias: ", show b'']
         loop (i + 1) w'' b''
  loop 0 w b
