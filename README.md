# Haskell Sudoku Solver

This repository contains a complete Sudoku solver implemented in Haskell. It leverages functional programming principles to recursively solve Sudoku puzzles by deriving potential values and systematically checking board states.

## ✨ Features

*   **Recursive Solving:** Implements a recursive backtracking approach to find solutions for given Sudoku boards.
*   **Derivation Logic:** Utilizes complex logic (`Helper.hs`) to calculate derived values based on the current board state, significantly pruning the search space.
*   **Data Structure Management:** Uses a custom `Sudoku` data type (`Types.hs`) to cleanly manage the board state, including individual cell values, row, column, and quadrant constraints.
*   **Constraint Checking:** Includes functions to validate board states (`isValid`) to ensure the integrity of the puzzle.

## 🛠️ Technology Stack

*   **Language:** Haskell
*   **Dependencies:** Relies on standard Haskell libraries and packages (e.g., `Data.Array.Unboxed`, `Data.Sort`).
*   **Build System:** Managed via a Cabal file (`lambda-sudoku.cabal`).

## 📂 Project Structure

The application is organized into logical modules:

*   `app/Main.hs`: Entry point for the application.
*   `app/Solver.hs`: Contains the core solving logic, including the main `solve` function.
*   `app/Types.hs`: Defines all core data structures, such as `Sudoku`, `BoardPosition`, and `Value`.
*   `app/Helper.hs`: Houses utility functions for board manipulation, value derivation, and constraint checking.
*   `CHANGELOG.md` & `LICENSE`: Standard project files.

## 🚀 Getting Started

### Prerequisites

You need a working Haskell environment (GHC and Cabal/Stack).

### Installation

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/Grabiel-Emannuel/lambda-sudoku.git
    cd sudoku
    ```
2.  **Build the project:**
    Use your preferred Haskell build tool (e.g., Cabal) to build the executable:
    ```bash
    cabal build
    ```
    or
    ```bash
    stack build
    ```

### Usage

The main entry point is defined in `app/Main.hs`. Run the compiled executable to interact with the solver.

*To find the exact command, refer to `app/Main.hs`.*

## 💡 Core Logic Overview

The solving process in `Solver.hs` works by:
1.  Checking if the current board is solved.
2.  Calculating potential values (`derivateValue` in `Helper.hs`).
3.  Recursively trying to place these derived values into empty cells (`place` function) and recurring on the new board state.

## 🤝 Contributing

Contributions are welcome! Please feel free to fork the repository and submit pull requests.

## 📄 License

This project is licensed under the [LICENSE](LICENSE) license.
