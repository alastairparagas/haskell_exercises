module IterativeStochasticGradient where 

import Data.List (foldl')
import Numeric.AD (diff)

type Weight = Double
type Feature = Double
type Intercept = Double
type LearningRate = Double
type TargetValue = Double
type TrainingExample = ([Feature], TargetValue)
type Epoch = Int


hypothesis :: [Weight] 
            -> [Feature] 
            -> Intercept 
            -> Double
hypothesis features weights intercept = 
  foldl' (+) intercept 
    (map 
      (\(weight, feature) -> weight * feature) 
      (zip weights features))


trainManual :: LearningRate
            -> [Feature]
            -> TargetValue
            -> [Weight]
            -> Intercept
            -> ([Weight], Intercept)
trainManual learningRate features targetValue guessWeights guessIntercept = 
  let
    newIntercept :: Intercept
    newIntercept = 
      guessIntercept + (learningRate * errorTerm)
      where 
        errorTerm =
          (targetValue - (
            hypothesis features guessWeights guessIntercept
          ))
            
    newWeights :: [Weight]
    newWeights = 
      foldl' determineWeight [] (zip guessWeights features)
      where
        determineWeight weights params@(weight, feature) = 
          let 
            errorTerm newWeights = 
              (targetValue - (
                hypothesis features newWeights newIntercept
              ))
            newWeight newWeights (weight, feature) = 
              weight + (
                learningRate * errorTerm newWeights * feature
              )
          in weights ++ [
              newWeight 
                (weights ++ drop (length weights) guessWeights) 
                params
              ]
  in (newWeights, newIntercept)


train :: [TrainingExample]
      -> [Weight]
      -> Intercept 
      -> LearningRate 
      -> Epoch
      -> ([Weight], Intercept)
train [] [] intercept _ _ = 
  ([], intercept)
train [] startingWeights intercept _ _ = 
  (startingWeights, intercept)
train 
  ((features, targetValue):xs) startingWeights intercept learningRate epoch = 
  | epoch == 1 = 
    let
      (newWeights, newIntercept) = 
        trainManual learningRate features targetValue startingWeights intercept
    in train xs newWeights newIntercept learningRate 1
  | epoch > 1 = 
    let
      (newWeights, newIntercept) = 
        train trainingSamples startingWeights intercept learningRate 1
    in train trainingSamples newWeights newIntercept learningRate (epoch-1)
  | otherwise = 
    (startingWeights, intercept)
