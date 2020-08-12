import qualified Numeric.LinearAlgebra as LA
import qualified Numeric.LinearAlgebra.Data as D (tr,  (><))

type X_Train = LA.Matrix Double
type Y_Train = LA.Matrix Double
type W = Double
type B = Double
type Output = LA.Matrix Double
type LearningRate = Double
-- | y = w * x + b for a single training example

ones :: Int -> Int -> LA.Matrix Double
ones x y = (x D.>< y) [1,1 ..]

output' :: W -> X_Train -> B -> Output
output' w xTrain b = w `LA.scale` xTrain + b `LA.scale` ones (fst (LA.size xTrain)) 1

cost :: Output -> Y_Train -> Double
cost output yTrain = LA.sumElements $ (1/2) * (output - yTrain) ^ 2

deltaCW' :: W -> X_Train -> B -> Double
deltaCW' w xTrain b = LA.sumElements $ D.tr (output' w xTrain b) <> xTrain

deltaCB' :: W -> X_Train -> Y_Train -> B -> Double
deltaCB' w xTrain yTrain b = LA.sumElements $ (output' w xTrain b - yTrain)

updateW' :: W -> X_Train -> B -> LearningRate -> Double
updateW' w xTrain b alpha = w - alpha * deltaCW' w xTrain b

updateB' :: W -> X_Train -> Y_Train -> B -> LearningRate -> Double
updateB' w xTrain yTrain b alpha = b - alpha * deltaCB' w xTrain yTrain b

test = do
  xTrain <- LA.rand 3 1
  yTrain <- LA.rand 3 1
  let w = 0
  let b = 0
  let alpha = 0.00001
  let loop i w' b' | i > 600 = return ()
      loop i w' b' | i <= 600 = do
         let c = cost (output' w' xTrain b') yTrain
         putStrLn $ concat ["Cost: ", show c]
         let w'' = updateW' w' xTrain b' alpha
         let b'' = updateB' w' xTrain yTrain b' alpha
         loop (i + 1) w'' b''
  loop 0 w b




{- ==== BAD CODE ====
Only for my own learning

type W = LA.Matrix Double
type B = LA.Matrix Double


output :: W -> X_Train -> B -> Output
output w xTrain b = (D.tr (w) <> xTrain) + b

deltaCW :: W -> X_Train -> Y_Train -> B -> LA.Matrix Double
deltaCW w xTrain yTrain b = ((D.tr (w) <> xTrain) + b) <> xTrain

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
-}
