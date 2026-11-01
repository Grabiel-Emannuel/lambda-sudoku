module Main (main) where

import Control.Exception (IOException, evaluate, try)
import Data.Array.Unboxed ((!))
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Solver (solve)
import Types (Sudoku(Board))
import IOHelper (ioHelper)

main :: IO ()
main = do
  arguments <- getArgs
  case arguments of
    ["--help"] -> putStrLn usage
    ["-h"] -> putStrLn usage
    _ -> case optionsFromArguments arguments of
      Left message -> putStrLn message >> exitFailure
      Right (path, maximumSolutions) -> processPath path maximumSolutions

processPath :: String -> Int -> IO ()
processPath path maximumSolutions = do
  result <- try (ioHelper path >>= evaluate)
    :: IO (Either IOException (Either String Sudoku))
  case result of
    Left exception -> do
      putStrLn ("Could not read " ++ path ++ ": " ++ show exception)
      exitFailure
    Right (Left message) -> do
      putStrLn ("Could not parse Sudoku: " ++ message)
      exitFailure
    Right (Right sudoku) -> case take maximumSolutions (solve sudoku) of
      [] -> putStrLn "This Sudoku has no solution."
      solutions -> mapM_ printSolution (zip [1 :: Int ..] solutions)

optionsFromArguments :: [String] -> Either String (String, Int)
optionsFromArguments ["--path", path] = Right (path, 100)
optionsFromArguments ["--path", path, limit] = withLimit path limit
optionsFromArguments [path] = Right (path, 100)
optionsFromArguments [path, limit] = withLimit path limit
optionsFromArguments _ = Left usage

withLimit :: String -> String -> Either String (String, Int)
withLimit path value = do
  limit <- parseLimit value
  Right (path, limit)

parseLimit :: String -> Either String Int
parseLimit value = case reads value of
  [(limit, "")] | limit > 0 -> Right limit
  _ -> Left ("Invalid maximum number of solutions: " ++ value ++ "\n" ++ usage)

usage :: String
usage = "Usage: lambda-sudoku --path <file.csv> [maximum-solutions]\n       lambda-sudoku <file.csv> [maximum-solutions]"

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
