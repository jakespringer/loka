{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

------------------------------------------------------------------------------
module Types where

------------------------------------------------------------------------------
data Roll = Roll Integer deriving (Show, Read)

------------------------------------------------------------------------------
data PieceDirection = DirectionRight | DirectionUpRight | DirectionUp
  | DirectionUpLeft | DirectionLeft | DirectionDownLeft | DirectionDown
  | DirectionDownRight deriving (Show, Read)
data KnightDirection = KnightDirectionUpRight | KnightDirectionUpLeft
  | KnightDirectionRightUp | KnightDirectionRightDown
  | KnightDirectionLeftUp | KnightDirectionLeftDown
  | KnightDirectionDownRight | KnightDirectionDownLeft deriving (Show, Read)
data MountainPassDirection = MountainPassDirectionHorizontal
  | MountainPassDirectionVertical deriving (Show, Read)

------------------------------------------------------------------------------
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn
  deriving (Show, Read)
data PieceColor = Red | Green | Blue | Yellow deriving (Show, Read)
data PieceMove = KingMove { direction :: PieceDirection }
  | QueenMove { direction :: PieceDirection, distance :: Integer }
  | RookMove { direction :: PieceDirection, distance :: Integer }
  | BishopMove { direction :: PieceDirection, distance :: Integer }
  | KnightMove { knightDirection :: KnightDirection }
  | PawnMove deriving (Show, Read)
data Piece = Piece { pieceX :: Integer
  , pieceY :: Integer
  , pieceType :: PieceType
  , color :: PieceColor } deriving (Show, Read)

------------------------------------------------------------------------------
data TerrainType = Eyrie | Castle | Swamp | MountainPass MountainPassDirection
  | Forest | Lake | StoneCircle | Portal deriving (Show, Read)
data Terrain = Terrain { terrainX :: Integer
  , terrainY :: Integer
  , terrainType :: TerrainType } deriving (Show, Read)

------------------------------------------------------------------------------
data GameMove = Noop
  | PlacePiece { piece :: Piece }
  | PlaceTerrain { terrain :: Terrain }
  | MovePiece { move :: PieceMove, piece :: Piece }
  | AttackPiece { move :: PieceMove
    , attacker :: Piece
    , defender :: Piece
    , attackerRoll :: Roll
    , defenderRoll :: Roll }
  | MultiMove [GameMove] deriving (Show, Read)

------------------------------------------------------------------------------
type Actor = String
type UnitAction = (Actor, GameMove)
type GameState = [UnitAction]
type CollapsedGameState = [Either Piece Terrain]
