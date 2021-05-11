# mineswept

Mineswept is:

- A runner for Minesweeper games.
  - `mineswept` can be used to run other Minesweeper programs.
- A generator of Minesweeper games.
  - Can generate guaranteed-solvable games.
- A solver of Minesweeper games.
  - Optimized for least clicks.
  - Optimized for simplest code.

## Usage

```sh
# Create a new game.
mineswept new --width=30 --height=16 --mines=99

# Perform an action.
mineswept --file=game1.mines dig x y
mineswept --file=game1.mines flag x y
```

Games are stored in a basic file format. Games are generated with 1 frame (the initial frame).

- Start with the parameters, line separated:
  - Width
  - Height
  - Total buried mines
  - RNG seed
  - `mineswept` data format version
- Then, divider
- Then, each turn has a frame (line separated), separated by dividers
  - Game status, one of:
    - Won
    - Lost
    - Playing
  - Timestamp when frame was committed
  - Number of remaining mines
  - Lines of squares (_H_ lines of _W_ length), one of:
    - ?: unknown
    - 0-9: numbers
    - F: flagged
    - X: exploded mine (cause of loss)
