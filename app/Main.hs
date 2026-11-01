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
    _ -> case pathFromArguments arguments of
      Left message -> putStrLn message >> exitFailure
      Right path -> processPath path

processPath :: String -> IO ()
processPath path = do
  result <- try (ioHelper path >>= evaluate)
    :: IO (Either IOException (Either String Sudoku))
  case result of
    Left exception -> do
      putStrLn ("Could not read " ++ path ++ ": " ++ show exception)
      exitFailure
    Right (Left message) -> do
      putStrLn ("Could not parse Sudoku: " ++ message)
      exitFailure
    Right (Right sudoku) -> case solve sudoku of
      [] -> putStrLn "This Sudoku has no solution."
      solutions -> mapM_ printSolution (zip [1 :: Int ..] solutions)

pathFromArguments :: [String] -> Either String String
pathFromArguments ["--path", path] = Right path
pathFromArguments [path] = Right path
pathFromArguments _ = Left usage

usage :: String
usage = "Usage: lambda-sudoku --path <file.csv>\n       lambda-sudoku <file.csv>"

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
