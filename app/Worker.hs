module Worker (worker) where

import Control.Concurrent.STM
import Control.Monad (forM_)
import Types (Sudoku, Workload)
import Solver (derivateValue, fill, isSolved, isValid, isPosValid, place)

worker :: TBQueue (Maybe Workload) -> TBQueue (Maybe Sudoku) -> TVar Int -> Int -> IO ()
worker jobs results pending workerCount = do
  nextJob <- atomically $ readTBQueue jobs
  case nextJob of
    Nothing -> atomically (writeTBQueue results Nothing)
    Just (board, position, values) -> do
      forM_ values $ \value ->
        solveShared jobs results pending (place board position value)
      finishJob
      worker jobs results pending workerCount
  where
    finishJob = atomically $ do
      remaining <- readTVar pending
      let remaining' = remaining - 1
      writeTVar pending remaining'
      if remaining' == 0
        then mapM_ (const (writeTBQueue jobs Nothing)) [1..workerCount]
        else pure ()

solveShared ::
  TBQueue (Maybe Workload) -> TBQueue (Maybe Sudoku) -> TVar Int -> Sudoku -> IO ()
solveShared jobs results pending board
  | not (isValid board)  = pure ()
  | isSolved board       = emit board
  | not (isValid filled) = pure ()
  | isSolved filled      = emit filled
  | not (isPosValid pos) = pure ()
  | otherwise =
      case values of
        [] -> pure ()
        (first:rest) -> do
          published <- atomically $ do
            full <- isFullTBQueue jobs
            if full
              then pure False
              else do
                writeTBQueue jobs (Just (filled, position, rest))
                modifyTVar' pending (+ 1)
                pure True
          if published
            then solveValue first
            else forM_ (first:rest) solveValue
  where
    filled = fill board
    pos@(position, values) = derivateValue filled
    emit solution = atomically $ writeTBQueue results (Just solution)
    solveValue value = solveShared jobs results pending (place filled position value)
