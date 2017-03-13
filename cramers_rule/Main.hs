module CramersRule where

import Control.Parallel.Strategies (parMap, rseq)
import Data.Matrix 
  (fromLists
    , nrows
    , ncols
    , detLU
    , setElem
    , getCol
    , submatrix
    , Matrix
  )
import Data.Maybe (isJust)
import Data.List (foldl')
import Data.Vector ((!), Vector)
import Text.CSV (parseCSVFromFile, CSV)
import Text.Parsec.Error (ParseError)
import Text.Read (readMaybe)
import System.IO (getLine, putStrLn, FilePath)
import System.Exit (die)
import System.Directory (doesFileExist)


askForFileLocation :: IO FilePath
askForFileLocation = 
  let
    handleResponse :: FilePath -> IO FilePath
    handleResponse userResponse = 
      let
        stateMachine :: FilePath -> Bool -> IO FilePath
        stateMachine fileName = 
          (\doesExist -> 
            case doesExist of 
              True -> return fileName
              False -> do
                putStrLn "File does not exist!"
                askForFileLocation
          )
      in doesFileExist(userResponse) >>= stateMachine userResponse
  in getLine >>= handleResponse


processCsvToMatrix :: FilePath -> IO (Matrix Double)
processCsvToMatrix filePath = 
  let 
    stateMachine :: Either ParseError CSV -> IO (Matrix Double)
    stateMachine (Left parseError) = 
      die "Not a proper CSV file!" 
    stateMachine (Right csv) = 
      return $ fromLists $ map (map (
        \field -> case (readMaybe field :: Maybe Double) of 
          Just n -> n
          Nothing -> 0
      )) csv 
  in parseCSVFromFile filePath >>= stateMachine


verifyValidMatrix :: Matrix Double -> Bool
verifyValidMatrix matrix = 
  (nrows matrix == (ncols matrix + 1)) && (ncols matrix >= 2)
  
  
matrixToSystemAndResult :: Matrix Double -> (Matrix Double, Vector Double)
matrixToSystemAndResult matrix = (
    submatrix 1 (nrows matrix) 1 (ncols matrix - 1) matrix, 
    getCol (ncols matrix) matrix
  )
  

evaluateCramerParallel :: Matrix Double -> Vector Double -> [Double]
evaluateCramerParallel squareMatrix resultVector = 
  let
    numCols = ncols squareMatrix
    numRows = nrows squareMatrix
    
    detOriginal = detLU squareMatrix
    
    cramersRule :: (Int, Double) -> Double
    cramersRule (currentCol, detOriginal) = 
      let 
        modifyMatrixFold :: Matrix Double
                          -> Int
                          -> Matrix Double
        modifyMatrixFold matrix currentRow = 
          let 
            resultAtRow = resultVector ! currentRow 
          in setElem 
              resultAtRow (currentRow, currentCol) matrix
        
        modifiedMatrix :: Matrix Double
        modifiedMatrix = 
          foldl' modifyMatrixFold squareMatrix [1..numRows]
      
      in (detLU modifiedMatrix) / detOriginal
  
  in parMap rseq cramersRule (
    zip [1..numCols] (take numCols (repeat detOriginal))
  )
  
  
computationPrompt :: Matrix Double -> IO ()
computationPrompt matrix = 
  let 
    solvedXs = 
      (uncurry evaluateCramerParallel) (
        matrixToSystemAndResult matrix
      )
  in putStrLn $ show $ solvedXs
  

main :: IO ()
main = do
  fileName <- askForFileLocation
  matrix <- processCsvToMatrix fileName
  case (verifyValidMatrix matrix) of 
    True -> computationPrompt matrix
    False -> do
      putStrLn "Invalid Matrix! Please provide a file with a valid matrix"
      main
