# Contributor Guide

## Project purpose

This repository contains a Haskell Sudoku solver. `Types` defines the board,
`CsvBoardReader` validates CSV data, `IOHelper` reads CSV files, and `Helper`
and `Solver` handle candidate derivation and solving.

## Language

Use English for all repository content, including documentation, source-code
comments, test names, test output, and contributor guidance. Keep new and
updated prose clear and consistent with the existing English documentation.

## Contribution practices

Read relevant code and tests first, keep changes focused and consistent with
the existing Haskell 2010 structure, and preserve interfaces and Sudoku/CSV
semantics unless the task requires changing them. Give each source module its
own corresponding test module; keep tests for different modules separate, and
store CSV fixtures in `test/fixtures/`. Update Cabal module or fixture
declarations when needed. Run `cabal test` when verification is requested or
appropriate.
