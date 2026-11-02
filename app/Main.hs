module Main (main) where

import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM
import Control.Exception (IOException, evaluate, try)
import Control.Monad (replicateM)
import Data.Array.Unboxed ((!))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Solver (derivateValue, fill, isPosValid, isSolved, isValid)
import Types (Sudoku(Board), Value)
import IOHelper (ioHelper)
import Worker (worker)

data Options = Options
  { inputPath :: Maybe String
  , solutionLimit :: Int
  , workerCount :: Int
  , positionalCount :: Int
  }

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    ["--help"] -> putStrLn usage
    ["-h"] -> putStrLn usage
    _ -> case optionsFromArguments arguments of
      Left message -> putStrLn message >> exitFailure
      Right (path, maximumSolutions, depth) -> processPath path maximumSolutions depth

processPath :: String -> Int -> Int -> IO ()
processPath path maximumSolutions depth = do
  result <- try (ioHelper path >>= evaluate)
    :: IO (Either IOException (Either String Sudoku))
  case result of
    Left exception -> do
      putStrLn ("Could not read " ++ path ++ ": " ++ show exception)
      exitFailure
    Right (Left message) -> do
      putStrLn ("Could not parse Sudoku: " ++ message)
      exitFailure
    Right (Right sudoku)
      | not (isValid sudoku) -> putStrLn "This Sudoku has no solution."
      | isSolved sudoku -> printSolution (1, sudoku)
      | otherwise -> do
          let prepared = fill sudoku
              (position, values) = derivateValue prepared
          if not (isValid prepared)
            then putStrLn "This Sudoku has no solution."
            else if isSolved prepared
              then printSolution (1, prepared)
              else if not (isPosValid (position, values))
                then putStrLn "This Sudoku has no solution."
                else runWorkers prepared position values maximumSolutions depth

runWorkers :: Sudoku -> (Int, Int) -> [Value] -> Int -> Int -> IO ()
runWorkers sudoku position values maximumSolutions depth = do
  jobs <- newTBQueueIO (fromIntegral (max 1 depth * 2))
  results <- newTBQueueIO (fromIntegral (maximumSolutions + depth))
  pending <- newTVarIO 1
  atomically $ writeTBQueue jobs (Just (sudoku, position, values))
  workerThreads <- replicateM depth (forkIO (worker jobs results pending depth))
  solutions <- collectResults results maximumSolutions depth 0 []
  if null solutions
    then putStrLn "This Sudoku has no solution."
    else mapM_ printSolution (zip [1 :: Int ..] solutions)
  mapM_ killThread workerThreads

collectResults :: TBQueue (Maybe Sudoku) -> Int -> Int -> Int -> [Sudoku] -> IO [Sudoku]
collectResults results maximumSolutions totalWorkers finished solutions
  | length solutions >= maximumSolutions = pure solutions
  | finished >= totalWorkers = pure solutions
  | otherwise = do
      result <- atomically (readTBQueue results)
      case result of
        Nothing -> collectResults results maximumSolutions totalWorkers (finished + 1) solutions
        Just sudoku -> collectResults results maximumSolutions totalWorkers finished (solutions ++ [sudoku])

optionsFromArguments :: [String] -> Either String (String, Int, Int)
optionsFromArguments arguments = do
  options <- parseOptions arguments (Options Nothing 100 1 0)
  case inputPath options of
    Nothing -> Left usage
    Just path -> Right (path, solutionLimit options, workerCount options)

parseOptions :: [String] -> Options -> Either String Options
parseOptions [] options = Right options
parseOptions ["--path"] _ = Left usage
parseOptions ["--solutions"] _ = Left usage
parseOptions ["--depth"] _ = Left usage
parseOptions ("--path":path:rest) options =
  if hasPath options
    then Left usage
    else parseOptions rest options { inputPath = Just path }
parseOptions ("--solutions":value:rest) options = do
  limit <- parsePositive "maximum number of solutions" value
  parseOptions rest options { solutionLimit = limit }
parseOptions ("--depth":value:rest) options = do
  depth <- parsePositive "worker depth" value
  parseOptions rest options { workerCount = depth }
parseOptions (argument:rest) options = case inputPath options of
  Nothing -> parseOptions rest options { inputPath = Just argument }
  Just _ -> case positionalCount options of
    0 -> do
      limit <- parsePositive "maximum number of solutions" argument
      parseOptions rest options { solutionLimit = limit, positionalCount = 1 }
    1 -> do
      depth <- parsePositive "worker depth" argument
      parseOptions rest options { workerCount = depth, positionalCount = 2 }
    _ -> Left usage

hasPath :: Options -> Bool
hasPath options = case inputPath options of
  Nothing -> False
  Just _ -> True

parsePositive :: String -> String -> Either String Int
parsePositive label value = case reads value of
  [(number, "")] | number > 0 -> Right number
  _ -> Left ("Invalid " ++ label ++ ": " ++ value ++ "\n" ++ usage)

usage :: String
usage = "Usage: lambda-sudoku --path <file.csv> [--solutions <count>] [--depth <workers>]\n       lambda-sudoku <file.csv> [maximum-solutions] [depth]"

printSolution :: (Int, Sudoku) -> IO ()
printSolution (number, Board board _ _ _ dimension _) = do
  putStrLn ("Solution " ++ show number ++ ":")
  mapM_ printRow
    [[show (fromIntegral (board ! (line, column)) :: Int)
     | column <- [1..dimension]]
    | line <- [1..dimension]]
  where
    printRow = putStrLn . joinWithComma
    joinWithComma [] = ""
    joinWithComma [cell] = cell
    joinWithComma (cell:cells) = cell ++ "," ++ joinWithComma cells
