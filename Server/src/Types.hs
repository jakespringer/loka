{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

------------------------------------------------------------------------------
module Types where

import Data.Aeson
import GHC.Generics

------------------------------------------------------------------------------
data Roll = Roll Integer deriving (Show, Read, Generic, ToJSON, FromJSON)

------------------------------------------------------------------------------
data PieceDirection = DirectionRight | DirectionUpRight | DirectionUp
  | DirectionUpLeft | DirectionLeft | DirectionDownLeft | DirectionDown
  | DirectionDownRight deriving (Show, Read, Generic, ToJSON, FromJSON)
data KnightDirection = KnightDirectionUpRight | KnightDirectionUpLeft
  | KnightDirectionRightUp | KnightDirectionRightDown
  | KnightDirectionLeftUp | KnightDirectionLeftDown
  | KnightDirectionDownRight | KnightDirectionDownLeft deriving (Show, Read, Generic, ToJSON, FromJSON)
data MountainPassDirection = MountainPassDirectionHorizontal
  | MountainPassDirectionVertical deriving (Show, Read, Generic, ToJSON, FromJSON)

------------------------------------------------------------------------------
data PieceType = King | Queen | Rook | Bishop | Knight | Pawn
  deriving (Show, Read, Generic, ToJSON, FromJSON)
data PieceColor = Red | Green | Blue | Yellow deriving (Show, Read, Generic, ToJSON, FromJSON)
data PieceMove = KingMove { direction :: PieceDirection }
  | QueenMove { direction :: PieceDirection, distance :: Integer }
  | RookMove { direction :: PieceDirection, distance :: Integer }
  | BishopMove { direction :: PieceDirection, distance :: Integer }
  | KnightMove { knightDirection :: KnightDirection }
  | PawnMove deriving (Show, Read, Generic, ToJSON, FromJSON)
data Piece = Piece { pieceX :: Integer
  , pieceY :: Integer
  , pieceType :: PieceType
  , color :: PieceColor } deriving (Show, Read, Generic, ToJSON, FromJSON)

------------------------------------------------------------------------------
data TerrainType = Eyrie | Castle | Swamp | MountainPass MountainPassDirection
  | Forest | Lake | StoneCircle | Portal deriving (Show, Read, Generic, ToJSON, FromJSON)
data Terrain = Terrain { terrainX :: Integer
  , terrainY :: Integer
  , terrainType :: TerrainType } deriving (Show, Read, Generic, ToJSON, FromJSON)

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
  | MultiMove [GameMove] deriving (Show, Read, Generic, ToJSON, FromJSON)

------------------------------------------------------------------------------
type Actor = String
type UnitAction = (Actor, GameMove)
type GameState = [UnitAction]
type CollapsedGameState = [Either Piece Terrain]
