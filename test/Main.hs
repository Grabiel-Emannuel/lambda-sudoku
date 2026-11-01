module Main (main) where

import CsvReaderTest (tests)
import qualified IOHelperTest

main :: IO ()
main = do
  fileTests <- IOHelperTest.tests
  let allTests = tests ++ fileTests
  mapM_ report allTests
  if all snd allTests
    then putStrLn ("All " ++ show (length allTests) ++ " tests passed.")
    else fail "One or more tests failed."
  where
    report (name, passed) =
      putStrLn ((if passed then "[OK] " else "[FAIL] ") ++ name)
