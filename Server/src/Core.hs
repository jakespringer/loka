module Loka.Core where

data Roll = Roll Integer

data PieceDirection = Right | UpRight | Up | UpLeft | Left | DownLeft | Down | DownRight

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn

data PieceColor = Red | Green | Green | Yellow

data PieceMove = KingMove { direction :: PieceDirection }
               | QueenMove { direction :: PieceDirection, distance :: Integer }
               | RookMove { direction :: PieceDirection, distance :: Integer }
               | BishopMove { direction :: PieceDirection, distance :: Integer }
               | KnightMove { direction :: PieceDirection }
               | PawnMove

data Piece = Piece { x :: Integer
                   , y :: Integer
                   , type :: PieceType
                   , color :: PieceColor }

data TerrainType = Eyrie | Castle | Swamp | MountainPass | Forest | Lake | StoneCircle | Portal

data Terrain = Terrain { x :: Integer
                       , y :: Integer
                       , type :: TerrainType }

data GameMove = Noop
              | PlacePiece { piece :: Piece }
              | PlaceTerrain { terrain :: Terrain }
              | MovePiece { piece :: Piece, move :: PieceMove }
              | AttackPiece { attacker :: Piece
                            , defender :: Piece
                            , move :: PieceMove
                            , attackerRoll :: Roll
                            , defenderRoll :: Roll }
              | InteractWithTerrain { piece :: Piece
                                    , terrain :: Terrain
                                    , }
              | MultiMove [GameMove]

type GameState = [GameMove]

type StaticGameState = [Either Piece Terrain]
