module IterativeStochasticGradient where 

{-
  Train is a function that accepts a:
  - a list of training example that consists of a list 
    of input features and a target value
  - a list of starting weights for each input feature
  - a learning rate
  Outputs a list of weights for each corresponding input 
  feature
-}
train :: [([Double], Double)] -> [Double] -> Double -> [Double]
train [] [] _ = []
train [] startingWeights _ = startingWeights
train 
  ((inputFeatures, targetValue):xs) startingWeights learningRate =
  
  let
    {-
      determineWeight :: [Double] -> (Double, Double) -> Double
      determineWeight calculates the weight of a single input feature for 
      a single training example. It accepts:
      - a list of current weights (ordered respective to the input features)
      - a tuple of starting weight and input feature value for the current 
        input feature we are to work on
      Returns the adjusted weight
    -}
    determineWeight 
      originWeights (originWeight, inputFeature) = 
      
      let predictedTargetValue = 
            foldl (+) 0 
            (map 
              (\(weight, feature) -> weight * feature) 
              (zip originWeights inputFeatures))
      
      in (originWeight + learningRate * 
        (targetValue - predictedTargetValue) * inputFeature)
      
    {-
      determineWeightsReducer :: [Double] -> (Double, Double) -> [Double]
      determineWeightsReducer iterates over every input feature of a 
      training example, calculating the new weight of an input feature 
      (as specified in the SGD algorithm)
    -}
    determineWeightsReducer weightsAccum params = 
      weightsAccum ++
      [determineWeight 
        (weightsAccum ++ (drop (length weightsAccum) startingWeights))
        params]
    
    
    determineWeights = 
      foldl 
        (determineWeightsReducer) 
        [] 
        (zip startingWeights inputFeatures)
  
  in (train xs determineWeights learningRate)
