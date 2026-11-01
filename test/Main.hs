module Main (main) where

import CsvReaderTest (tests)

main :: IO ()
main = do
  mapM_ report tests
  if all snd tests
    then putStrLn ("Todos os " ++ show (length tests) ++ " testes passaram.")
    else fail "Um ou mais testes falharam."
  where
    report (name, passed) =
      putStrLn ((if passed then "[OK] " else "[FALHOU] ") ++ name)
