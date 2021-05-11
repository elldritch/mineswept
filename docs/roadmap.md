# Roadmap

## Future commands

```sh
# Create a new game.
mineswept new --width=30 --height=16 --mines=99 --guaranteed-solvable

# Perform an action.
mineswept --file=game1.mines dig x y
mineswept --file=game1.mines flag x y
mineswept --file=game1.mines cascade x y

# Generate the optimal next actions. Might be more than one action (with odds) if guessing is required.
mineswept --file=game1.mines suggest

# Reveal the underlying minefield.
mineswept --file=game1.mines give-up
```
