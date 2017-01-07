# Loka Server

### Dependencies
- Haskell
- Cabal

### To run
```
cabal run
```

### Notes
Algorithm to check the validity of a given state-action pair:
- Subroutine "Check if tile allows move"
  - check currentTile
    - case Eyrie
      - case Knight
        = True
      - other
        = False
    - case Castle
      = True
    - case Swamp
      = True
    -
  - check previousTile
    - case Swamp
      = False

- case Move
  - case Knight
    - Jump to target square
    - Check if valid square
      - Not occupied, not off-world
    - Check if tile allows move
  - case King
    - Move to target square
    - Check if valid square
      - Not occupied, not off-world
    - Check if not in check
    - Check if the tile allows move
  - case Queen
    - Move one square at a time to target square; for each square
      - Check if valid square
        - Not occupied, not off-world
      - Check if the tile allows move given the previous square occupied
  - case Rook
    - Same as Queen
  - case Bishop
    - Same as Queen
  - case Pawn
    - Move to target square
    - Check if valid square
      - Not occupied (pawn swap requires "swap" action), not off-world
    - Check if not at end of the world (requires promote)
