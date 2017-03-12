module IterativeStochasticGradient where 

import Data.List (foldl')
import Data.Functor (fmap)

{-
  Train is a function that accepts a:
  - a list of training examples that consists of a list 
    of input features and a target value
  - a list of starting weights for each input feature
  - an intercept value
  - a learning rate
  Outputs a tuple: list of weights for each corresponding input 
  feature and an intercept value
-}
train 
  :: [([Double], Double)] 
  -> [Double] 
  -> Double 
  -> Double 
  -> ([Double], Double)
train [] [] intercept _ = 
  ([], intercept)
train [] startingWeights intercept _ = 
  (startingWeights, intercept)
train 
  ((features, targetValue):xs) startingWeights intercept learningRate =
  
  let
    hypothesis :: [Double] -> [Double] -> Double -> Double
    hypothesis weights features intercept = 
      foldl' (+) intercept 
        (fmap 
          (\(weight, feature) -> weight * feature) 
          (zip weights features))
    
    determineWeight :: [Double] -> (Double, Double) -> Double
    determineWeight 
      originWeights (originWeight, feature) = 
      let
        errorDifference = 
          (targetValue - (hypothesis originWeights features intercept))
      in originWeight + (learningRate * errorDifference * feature)
    
    determineWeightsFold :: [Double] -> (Double, Double) -> [Double]
    determineWeightsFold newWeights params@(originWeight, feature) = 
      newWeights ++
      [determineWeight 
        (newWeights ++ (drop (length newWeights) startingWeights))
        params]
    
    newWeights :: [Double]
    newWeights = 
      foldl'  
        (determineWeightsFold) 
        [] 
        (zip startingWeights features)
    
    newIntercept :: Double
    newIntercept = 
      let
        errorDifference = 
          (targetValue - (hypothesis startingWeights features intercept))
      in intercept + (learningRate * errorDifference)
  
  in (train xs newWeights newIntercept learningRate)
