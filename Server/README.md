# Loka Server

### Dependencies
- Haskell
- Cabal

### To install
(In development mode)
```
git clone <see git clone link>
cd loka/Server
cabal sandbox init
cabal install
```

### To run
```
cabal run
```

### API

Input
```
POST /game/<gamename>/action
{
  "actor": "<username/color?>",
  "type": "QueenMove",
  "piece": {
    "x": 4,
    "y": 3,
    "color": "Red",
    "pieceType": "Queen"
  },
  "direction": "UpRight",
  "distance": 4,
  "actionIndex": 24
}
```

Output
```
Identical to GET /game/<gamename>/state
```

Errors
```
{
  "status": "error",
  "code": 1,
  "message": "You are unauthenticated"
}
```
```
{
  "status": "error",
  "code": 2,
  "message": "Invalid action"
}
```

Input
```GET /game/<gamename>/state```

Output
```
{
  "status": "success",
  "actions": [
    {
      "actor": "<username/color?>",
      "type": "QueenMove",
      "piece": {
        "x": 4,
        "y": 3,
        "color": "Red",
        "pieceType": "Queen"
      },
      "direction": "UpRight",
      "distance": 4,
      "actionIndex": 0
    },
    ...
  ],
  "staticState": [
    ...,
    {
      "piece": {
        "x": 3,
        "y": 1,
        "color": "Blue",
        "pieceType": "Pawn"
      },
      "terrain": {
        "x": 3,
        "y": 1,
        "terrainType": "MountainPass",
        "mountainPassOrientation": "Horizontal"
      }
    },
    {
      "piece": null
      "terrain": null
    },
    ... (.length = boardSideLength^2)
  ]
}
```
Note: for static state, alternatively, there could be a list of only the existing pieces and terrain tiles.
