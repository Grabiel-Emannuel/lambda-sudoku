module Main (main) where

import CsvReaderTest (tests)
import qualified HelperTests
import qualified IOHelperTest
import qualified SolverTest

main :: IO ()
main = do
  fileTests <- IOHelperTest.tests
  let allTests = tests ++ fileTests ++ HelperTests.tests ++ SolverTest.tests
  mapM_ report allTests
  if all snd allTests
    then putStrLn ("All " ++ show (length allTests) ++ " tests passed.")
    else fail "One or more tests failed."
  where
    report (name, passed) =
      putStrLn ((if passed then "[OK] " else "[FAIL] ") ++ name)
