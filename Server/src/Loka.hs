{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing
    -fwarn-hi-shadowing -fno-warn-unused-matches
 #-}

------------------------------------------------------------------------------
-- | This module defines the rules of Loka
module Loka where

------------------------------------------------------------------------------
import Data.Maybe
import Data.List

import Types

------------------------------------------------------------------------------
-- | Returns a Just GameState with the appended
appendGameMove :: Actor -> GameMove -> GameState -> Maybe GameState
appendGameMove actor move state
  | checkMove move (collapseGameState state) = Just ((actor, move) : state)
  | otherwise = Nothing

------------------------------------------------------------------------------
-- | Collapses the GameState as a list of actions into a list of pieces in
-- their final location.
collapseGameState :: GameState -> CollapsedGameState
collapseGameState state = foldl collapseSingleState [] $ map (\(_, move) -> move) state
  where
    xyPieceNotEquals xCheck yCheck (Left (Piece x y _ _)) = x /= xCheck || y /= yCheck
    xyPieceNotEquals _ _ (Right _) = False
    xyTerrainNotEquals xCheck yCheck (Left (Terrain x y _)) = x /= xCheck || y /= yCheck
    xyTerrainNotEquals _ _ (Right _) = False

    straightLineMove dir dist x y pieceType pieceColor state =
      (Left $ uncurry Piece (pieceMoveCoordinates dir 1 (x, y)) pieceType pieceColor)
        : (filter (not . xyPieceEquals x y) state)

    collapseSingleState :: CollapsedGameState -> GameMove -> CollapsedGameState
    collapseSingleState state Noop = state
    collapseSingleState state (PlacePiece piece) = (Left piece) : state
    collapseSingleState state (PlaceTerrain terrain) = (Right terrain) : state
    collapseSingleState state (MovePiece (KingMove dir) (Piece x y pieceType pieceColor)) =
      straightLineMove dir 1 x y pieceType pieceColor state
    collapseSingleState state (MovePiece (QueenMove dir dist) (Piece x y pieceType pieceColor)) =
      straightLineMove dir dist x y pieceType pieceColor state
    collapseSingleState state (MovePiece (RookMove dir dist) (Piece x y pieceType pieceColor)) =
      straightLineMove dir dist x y pieceType pieceColor state
    collapseSingleState state (MovePiece (BishopMove dir dist) (Piece x y pieceType pieceColor)) =
      straightLineMove dir dist x y pieceType pieceColor state
    collapseSingleState state (MovePiece (KnightMove kDir) (Piece x y pieceType pieceColor)) =
      (Left $ uncurry Piece (knightMoveCoordinates kDir (x, y)) pieceType pieceColor)
        : (filter (not . xyPieceEquals x y) state)
    collapseSingleState state (MovePiece PawnMove (Piece x y pieceType pieceColor)) =
      straightLineMove (pawnColorDirection pieceColor) 1 x y pieceType pieceColor state
    collapseSingleState state _ = state

------------------------------------------------------------------------------
-- | Returns a list of all possible moves that can be made on the board for a
-- given color.
allPossibleMoves :: CollapsedGameState -> PieceColor -> [GameMove]
allPossibleMoves state pieceColor = concat $ map (allPossiblePieceTerrainMoves state) $
  filter (matchesColor pieceColor) state
  where
    allPossiblePieceTerrainMoves state (Left x) = allPossiblePieceMoves state x
    allPossiblePieceTerrainMoves _ _ = []

    matchesColor pieceColor (Left (Piece _ _ _ check)) = check == pieceColor
    matchesColor _ _ = False

------------------------------------------------------------------------------
-- | Returns a list of all possible moves that a given piece can make on the
-- board.
allPossiblePieceMoves :: CollapsedGameState -> Piece -> [GameMove]
allPossiblePieceMoves state piece = filter (\x -> checkMove x state) $
  constructMoves state piece
  where
    kingMovePiece piece dir = (MovePiece (KingMove dir)) piece
    queenMovePiece piece dir dist = (MovePiece (QueenMove dir dist)) piece
    rookMovePiece piece dir dist = (MovePiece (RookMove dir dist)) piece
    bishopMovePiece piece dir dist = (MovePiece (BishopMove dir dist)) piece
    knightMovePiece piece dir = (MovePiece (KnightMove dir)) piece

    constructMoves state (Piece x y Queen pieceColor) =
      concat $ map (\dir -> map (queenMovePiece (Piece x y Queen pieceColor) dir) [1..12])
        [ DirectionUp, DirectionUpRight, DirectionRight, DirectionDownRight
        , DirectionDown, DirectionDownLeft, DirectionLeft, DirectionUpLeft ]

    constructMoves state (Piece x y King pieceColor) =
      map (\dir -> kingMovePiece (Piece x y King pieceColor) dir)
       [ DirectionUp, DirectionUpRight, DirectionRight, DirectionDownRight
       , DirectionDown, DirectionDownLeft, DirectionLeft, DirectionUpLeft ]

    constructMoves state (Piece x y Rook pieceColor) =
      concat $ map (\dir -> map (rookMovePiece (Piece x y Rook pieceColor) dir) [1..12])
        [ DirectionUp, DirectionRight, DirectionDown, DirectionLeft ]

    constructMoves state (Piece x y Bishop pieceColor) =
      concat $ map (\dir -> map (bishopMovePiece (Piece x y Bishop pieceColor) dir) [1..12])
        [ DirectionUpRight, DirectionDownRight, DirectionDownLeft, DirectionUpLeft ]

    constructMoves state (Piece x y Knight pieceColor) =
      map (\dir -> knightMovePiece (Piece x y Knight pieceColor) dir)
        [ KnightDirectionLeftUp, KnightDirectionLeftDown
        , KnightDirectionRightUp, KnightDirectionRightDown
        , KnightDirectionUpRight, KnightDirectionUpLeft
        , KnightDirectionDownRight, KnightDirectionDownLeft]

    constructMoves state (Piece x y Pawn pieceColor) =
      [ MovePiece PawnMove (Piece x y Pawn pieceColor) ]

------------------------------------------------------------------------------
-- | Utility function that converts (x, y) coordinates to new coordinates based
-- on move made.
pieceMoveCoordinates :: PieceDirection -> Integer -> (Integer, Integer)
  -> (Integer, Integer)
pieceMoveCoordinates dir dist coord = (dist
  `vecTimes` pieceDirectionCoordinates dir)
  `vecAdd` coord

------------------------------------------------------------------------------
-- | Utility function that converts (x, y) coordinates to new coordinates based
-- on knight move made.
knightMoveCoordinates :: KnightDirection -> (Integer, Integer) -> (Integer, Integer)
knightMoveCoordinates kDir coord = coord
  `vecAdd` knightDirectionCoordinates kDir

------------------------------------------------------------------------------
-- | Utility function that converts a PieceDirection to its corresponding
-- coordinate change.
pieceDirectionCoordinates :: PieceDirection -> (Integer, Integer)
pieceDirectionCoordinates DirectionRight = (1, 0)
pieceDirectionCoordinates DirectionUpRight = (1, 1)
pieceDirectionCoordinates DirectionUp = (0, 1)
pieceDirectionCoordinates DirectionUpLeft = (-1, 1)
pieceDirectionCoordinates DirectionLeft = (-1, 0)
pieceDirectionCoordinates DirectionDownLeft = (-1, -1)
pieceDirectionCoordinates DirectionDown = (0, -1)
pieceDirectionCoordinates DirectionDownRight = (1, -1)

------------------------------------------------------------------------------
-- | Utility function that converts a KnightDirection to its corresponding
-- coordinate change.
knightDirectionCoordinates :: KnightDirection -> (Integer, Integer)
knightDirectionCoordinates KnightDirectionUpRight = (1, 2)
knightDirectionCoordinates KnightDirectionUpLeft = (-1, 2)
knightDirectionCoordinates KnightDirectionDownRight = (1, -2)
knightDirectionCoordinates KnightDirectionDownLeft = (-1, -2)
knightDirectionCoordinates KnightDirectionRightDown = (2, -1)
knightDirectionCoordinates KnightDirectionRightUp = (2, 1)
knightDirectionCoordinates KnightDirectionLeftDown = (-2, -1)
knightDirectionCoordinates KnightDirectionLeftUp = (-2, 1)

------------------------------------------------------------------------------
-- | Utility function that converts a PieceColor to its corresponding
-- pawn direction.
pawnColorDirection :: PieceColor -> PieceDirection
pawnColorDirection Red = DirectionUp
pawnColorDirection Blue = DirectionDown
pawnColorDirection Green = DirectionRight
pawnColorDirection Yellow = DirectionLeft

------------------------------------------------------------------------------
-- | Utility function that checks if a coordinate is in the board bounds.
inBoardBounds :: Integer -> Integer -> Bool
inBoardBounds x y = 0 <= x && x < 12 && 0 <= y && y < 12
  && ((4 <= x && x < 9) || (4 <= y && y < 9))

------------------------------------------------------------------------------
-- | Utility function that adds two vectors.
vecAdd :: Num a => (a, a) -> (a, a) -> (a, a)
vecAdd (x, y) (z, w) = (x+z, y+w)

------------------------------------------------------------------------------
-- | Utility function that multiplies a scalar and a vector.
vecTimes :: Num a => a -> (a, a) -> (a, a)
vecTimes scalar (x, y) = (x*scalar, y*scalar)

------------------------------------------------------------------------------
-- | Utility function that checks if you can enter certain terrain.
canEnterTerrain :: TerrainType -> PieceType -> PieceDirection -> Bool
canEnterTerrain Eyrie Knight _ = True
canEnterTerrain Eyrie _ _ = False
canEnterTerrain Castle _ _ = True
canEnterTerrain Swamp _ _ = True
canEnterTerrain (MountainPass _) Knight _ = True
canEnterTerrain (MountainPass MountainPassDirectionVertical) _ DirectionUp = True
canEnterTerrain (MountainPass MountainPassDirectionVertical) _ DirectionDown = True
canEnterTerrain (MountainPass MountainPassDirectionHorizontal) _ DirectionRight = True
canEnterTerrain (MountainPass MountainPassDirectionHorizontal) _ DirectionLeft = True
canEnterTerrain (MountainPass _) _ _ = False
canEnterTerrain Forest Pawn _ = True
canEnterTerrain Forest _ _ = False
canEnterTerrain Lake _ _ = False
canEnterTerrain StoneCircle Rook _ = True
canEnterTerrain StoneCircle _ _ = False
canEnterTerrain Portal _ _ = True

------------------------------------------------------------------------------
-- | UtilipieceType function that checks if you can exit certain terrain.
canExitTerrain :: TerrainType -> PieceType -> PieceDirection -> Bool
canExitTerrain Swamp _ _ = False
canExitTerrain _ _ _ = True

------------------------------------------------------------------------------
-- | A number of utilipieceType functions that are used in checkMove

-- Helper functions to check if a given piece/terrain has a given x,y
xyPieceEquals xCheck yCheck (Left (Piece x y _ _)) = x == xCheck && y == yCheck
xyPieceEquals _ _ (Right _) = False

xyTerrainEquals xCheck yCheck (Right (Terrain x y _)) = x == xCheck && y == yCheck
xyTerrainEquals _ _ (Left _) = False

-- Checks if a direction is cardinal
isCardinal :: PieceDirection -> Bool
isCardinal DirectionRight = True
isCardinal DirectionUpRight = False
isCardinal DirectionUp = True
isCardinal DirectionUpLeft = False
isCardinal DirectionLeft = True
isCardinal DirectionDownLeft = False
isCardinal DirectionDown = True
isCardinal DirectionDownRight = False

-- Checks if a direction is diagonal
isDiagonal = not . isCardinal

-- Returns a square of distance dist away from x,y in direction dir
lastSquare dir x y dist = (dist `vecTimes` pieceDirectionCoordinates dir)
  `vecAdd` (x, y)

-- Returns a list of all coordinates in a sraight line from x,y in direction dir
straightLine dir dist x y = map (lastSquare dir x y) [1..dist]

-- Checks if a given piece can enter a given square
canEnterSquare static pType dir x y = inBoardBounds x y
  && noPiece x y
  && goodTerrain x y
    where
      noPiece x y = isNothing $ find (xyPieceEquals x y) static
      goodTerrain x y = case find (xyTerrainEquals x y) static of
        Just (Right (Terrain _ _ tType)) -> canEnterTerrain tType pType dir
        Nothing -> True
        _ -> True

-- Checks if a given piece can exit a given square (used for Swap rule)
canExitSquare static pType dir x y = case find (xyTerrainEquals x y) static of
  Just (Right (Terrain _ _ tType)) -> canExitTerrain tType pType dir
  Nothing -> True
  _ -> True

-- Checks if a given piece can walk along every square in the given direction
canTraverseBoard :: Piece -> PieceDirection -> Integer -> CollapsedGameState -> Bool
canTraverseBoard (Piece x y pieceType _) dir dist static =
  (all (uncurry (canEnterSquare static pieceType dir)) $ straightLine dir dist x y)
    && (all (uncurry (canExitSquare static pieceType dir)) $ straightLine dir (dist - 1) x y)

------------------------------------------------------------------------------
-- | Checks if a given move is valid.
checkMove :: GameMove -> CollapsedGameState -> Bool
checkMove Noop _ = True
checkMove (PlacePiece (Piece x y pieceType pieceColor)) state = all (not . xyPieceEquals x y) state
checkMove (PlaceTerrain (Terrain x y pieceType)) state = all (not . xyTerrainEquals x y) state
checkMove (MovePiece (KingMove dir) (Piece x y King pieceColor)) state =
  canTraverseBoard (Piece x y King pieceColor) dir 1 state
checkMove (MovePiece (QueenMove dir dist) (Piece x y Queen pieceColor)) state =
  canTraverseBoard (Piece x y Queen pieceColor) dir dist state
checkMove (MovePiece (RookMove dir dist) (Piece x y Rook pieceColor)) state =
  isCardinal dir && checkCanTraverseBoard
    where
      checkCanTraverseBoard = canTraverseBoard (Piece x y Rook pieceColor) dir dist state
checkMove (MovePiece (BishopMove dir dist) (Piece x y Bishop pieceColor)) state =
  isDiagonal dir && checkCanTraverseBoard
    where
      checkCanTraverseBoard = canTraverseBoard (Piece x y Bishop pieceColor) dir dist state
checkMove (MovePiece (KnightMove kDir) (Piece x y Knight pieceColor)) state =
  (uncurry inBoardBounds) ((x, y) `vecAdd` (knightDirectionCoordinates kDir) )
    && (uncurry $ canEnterSquare state Knight DirectionUp) ((x, y)
      `vecAdd` (knightDirectionCoordinates kDir) )
checkMove (MovePiece PawnMove (Piece x y Pawn pieceColor)) state =
  canTraverseBoard (Piece x y Pawn pieceColor) (pawnColorDirection pieceColor) 1 state
checkMove _ _ = False
