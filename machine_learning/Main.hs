module Main where

import System.IO (getLine, readFile)
import System.Directory (doesFileExist)
import Text.Read (readMaybe)
import Data.Maybe (fromJust)

import IterativeStochasticGradient (train)

          
-- Main program execution
main :: IO ()
main = 
  
  let 
     -- ask file location
    askFileLocation :: IO String
    askFileLocation = do
      putStrLn 
          "Where is the path of the space-delimited training dataset?"
      fileLocation <- System.IO.getLine
      fileExists <- System.Directory.doesFileExist fileLocation
      case fileExists of
        True -> return fileLocation
        False -> do 
          putStrLn "Invalid File Path!"
          askFileLocation
    
    -- ask learning rate
    askLearningRate :: IO Double
    askLearningRate = do
      putStrLn
        "What learning rate would you want? (Must be a double)"
      learningRate_ <- System.IO.getLine
      case (readMaybe learningRate_ :: Maybe Double) of 
        Just x -> return x
        Nothing -> do
          putStrLn "Invalid Learning Rate!"
          askLearningRate
    
    -- Parse File Content into a list of training examples
    parseFileContent :: String -> [([Double], Double)]
    parseFileContent fileContent =
      let 
        fileLines = lines fileContent
        processLines line = 
          let 
            parsedLine = (map read (words line))
          in (init parsedLine ++ [1], last parsedLine)
        in (map processLines fileLines)
    
    -- Obtain predictionary weights provided training examples 
    -- and learning rate
    trainWithData :: [([Double], Double)] -> Double -> [Double]
    trainWithData [] learningRate = 
      IterativeStochasticGradient.train [] [] 0
    trainWithData 
      trainingData@((features, targetValue):xs) 
      learningRate = 
      let 
        startingWeights = (take (length features) (repeat 120))
        in IterativeStochasticGradient.train 
            trainingData startingWeights learningRate
        
    -- Drop the nth column (1-indexed) from input features of 
    -- training examples
    dropColumnFromFeatures ::
      [([Double], Double)] -> Int -> [([Double], Double)]
    dropColumnFromFeatures trainingExamples n = 
      map 
      (\(features, targetValue) -> 
        let (xs, ys) = splitAt (n-1) features
          in (xs ++ tail ys, targetValue)
      ) trainingExamples
  
    -- predict new weights provided old weights and input features
    predict :: [Double] -> [Double] -> Double
    predict weights features = 
      let
        addends = zip weights features
      in foldl (+) 0 
        (map (\(weight,feature) -> weight * feature) addends)
  
    in do
    fileLocation <- askFileLocation
    learningRate <- askLearningRate
    fileContent <- System.IO.readFile fileLocation
  
    let 
      trainingExamples = 
        parseFileContent fileContent

      trainingExamplesMod = 
        dropColumnFromFeatures trainingExamples 1

      weights = 
        trainWithData trainingExamplesMod learningRate

      in do
      putStrLn "Training Examples --->"
      mapM_ print trainingExamplesMod
      putStrLn "Output Weights --->"
      putStrLn $ show $ weights
      putStrLn "Target value guess for first training example --->"
      putStrLn $ show $ 
        predict weights (fst (trainingExamplesMod !! 0))
  