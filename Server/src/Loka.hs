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
appendGameMove :: Actor -> GameMove -> Maybe GameState -> Maybe GameState
appendGameMove _ _ Nothing = Nothing
appendGameMove actor move (Just state) = case collapseGameState (Just state) of
  Nothing -> Nothing
  Just static -> if checkMove move static
    then Just ((actor, move) : state)
    else Nothing

------------------------------------------------------------------------------
-- | Collapses the GameState as a list of actions into a list of pieces in
-- their final location.
collapseGameState :: Maybe GameState -> Maybe CollapsedGameState
collapseGameState Nothing = Nothing
collapseGameState (Just state) = Just $ foldl collapseSingleState [] $ map (\(_, move) -> move) state
  where
    xyPieceNotEquals xCheck yCheck (Left (Piece x y _ _)) = x /= xCheck || y /= yCheck
    xyPieceNotEquals _ _ (Right _) = False
    xyTerrainNotEquals xCheck yCheck (Left (Terrain x y _)) = x /= xCheck || y /= yCheck
    xyTerrainNotEquals _ _ (Right _) = False

    collapseSingleState :: CollapsedGameState -> GameMove -> CollapsedGameState
    collapseSingleState static Noop = static
    collapseSingleState static (PlacePiece piece) = (Left piece) : static
    collapseSingleState static (PlaceTerrain terrain) = (Right terrain) : static
    collapseSingleState static (MovePiece (KingMove dir) (Piece x y ty col)) =
      (Left $ uncurry Piece (pieceMoveCoordinates dir 1 (x, y)) ty col)
        : (filter (xyPieceNotEquals x y) static)
    collapseSingleState static (MovePiece (QueenMove dir dist) (Piece x y ty col)) =
      (Left $ uncurry Piece (pieceMoveCoordinates dir dist (x, y)) ty col)
        : (filter (xyPieceNotEquals x y) static)
    collapseSingleState static (MovePiece (RookMove dir dist) (Piece x y ty col)) =
      (Left $ uncurry Piece (pieceMoveCoordinates dir dist (x, y)) ty col)
        : (filter (xyPieceNotEquals x y) static)
    collapseSingleState static (MovePiece (BishopMove dir dist) (Piece x y ty col)) =
      (Left $ uncurry Piece (pieceMoveCoordinates dir dist (x, y)) ty col)
        : (filter (xyPieceNotEquals x y) static)
    collapseSingleState static (MovePiece (KnightMove kDir) (Piece x y ty col)) =
      (Left $ uncurry Piece (knightMoveCoordinates kDir (x, y)) ty col)
        : (filter (xyPieceNotEquals x y) static)
    collapseSingleState static (MovePiece PawnMove (Piece x y ty col)) =
      (Left $ uncurry Piece (pieceMoveCoordinates (pawnColorDirection col)
        1 (x, y)) ty col)
        : (filter (xyPieceNotEquals x y) static)
    collapseSingleState static _ = static

------------------------------------------------------------------------------
-- | Returns a list of all possible moves that can be made on the board for a
-- given color.
allPossibleMoves :: CollapsedGameState -> PieceColor -> [GameMove]
allPossibleMoves state col = concat $ map (allPossiblePieceTerrainMoves state) $
  filter (matchesColor col) state
  where
    allPossiblePieceTerrainMoves state (Left x) = allPossiblePieceMoves state x
    allPossiblePieceTerrainMoves _ _ = []

    matchesColor col (Left (Piece _ _ _ check)) = check == col
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

    constructMoves state (Piece x y Queen col) =
         map (queenMovePiece (Piece x y Queen col) DirectionUp) [1..12]
      ++ map (queenMovePiece (Piece x y Queen col) DirectionUpRight) [1..12]
      ++ map (queenMovePiece (Piece x y Queen col) DirectionRight) [1..12]
      ++ map (queenMovePiece (Piece x y Queen col) DirectionDownRight) [1..12]
      ++ map (queenMovePiece (Piece x y Queen col) DirectionDown) [1..12]
      ++ map (queenMovePiece (Piece x y Queen col) DirectionDownLeft) [1..12]
      ++ map (queenMovePiece (Piece x y Queen col) DirectionLeft) [1..12]
      ++ map (queenMovePiece (Piece x y Queen col) DirectionUpLeft) [1..12]

    constructMoves state (Piece x y King col) =
      [ kingMovePiece (Piece x y King col) DirectionUp
      , kingMovePiece (Piece x y King col) DirectionUpRight
      , kingMovePiece (Piece x y King col) DirectionRight
      , kingMovePiece (Piece x y King col) DirectionDownRight
      , kingMovePiece (Piece x y King col) DirectionDown
      , kingMovePiece (Piece x y King col) DirectionDownLeft
      , kingMovePiece (Piece x y King col) DirectionLeft
      , kingMovePiece (Piece x y King col) DirectionUpLeft]

    constructMoves state (Piece x y Rook col) =
         map (rookMovePiece (Piece x y Rook col) DirectionUp) [1..12]
      ++ map (rookMovePiece (Piece x y Rook col) DirectionRight) [1..12]
      ++ map (rookMovePiece (Piece x y Rook col) DirectionDown) [1..12]
      ++ map (rookMovePiece (Piece x y Rook col) DirectionLeft) [1..12]

    constructMoves state (Piece x y Bishop col) =
         map (bishopMovePiece (Piece x y Bishop col) DirectionUpRight) [1..12]
      ++ map (bishopMovePiece (Piece x y Bishop col) DirectionDownRight) [1..12]
      ++ map (bishopMovePiece (Piece x y Bishop col) DirectionDownLeft) [1..12]
      ++ map (bishopMovePiece (Piece x y Bishop col) DirectionUpLeft) [1..12]

    constructMoves state (Piece x y Knight col) =
      [ knightMovePiece (Piece x y Knight col) KnightDirectionLeftUp
      , knightMovePiece (Piece x y Knight col) KnightDirectionLeftDown
      , knightMovePiece (Piece x y Knight col) KnightDirectionRightUp
      , knightMovePiece (Piece x y Knight col) KnightDirectionRightDown
      , knightMovePiece (Piece x y Knight col) KnightDirectionUpRight
      , knightMovePiece (Piece x y Knight col) KnightDirectionUpLeft
      , knightMovePiece (Piece x y Knight col) KnightDirectionDownRight
      , knightMovePiece (Piece x y Knight col) KnightDirectionDownLeft]

    constructMoves state (Piece x y Pawn col) =
      [ MovePiece PawnMove (Piece x y Pawn col) ]


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
-- | Utility function that checks if you can exit certain terrain.
canExitTerrain :: TerrainType -> PieceType -> PieceDirection -> Bool
canExitTerrain Swamp _ _ = False
canExitTerrain _ _ _ = True


------------------------------------------------------------------------------
-- | A number of utility functions that are used in checkMove

-- Helper functions to check if a given piece/terrain has a given x,y
xyPieceEquals xCheck yCheck (Left (Piece x y _ _)) = x == xCheck && y == yCheck
xyPieceEquals _ _ (Right _) = False
xyTerrainEquals xCheck yCheck (Right (Terrain x y _)) = x == xCheck && y == yCheck
xyTerrainEquals _ _ (Left _) = False

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

-- Checks ifi a given piece can walk along every square in the given direction
canTraverseBoard :: Piece -> PieceDirection -> Integer -> CollapsedGameState -> Bool
canTraverseBoard (Piece x y ty _) dir dist static =
  (all (uncurry (canEnterSquare static ty dir)) $ straightLine dir dist x y)
    && (all (uncurry (canExitSquare static ty dir)) $ straightLine dir (dist - 1) x y)

------------------------------------------------------------------------------
-- | Checks if a given move is valid.
checkMove :: GameMove -> CollapsedGameState -> Bool
checkMove Noop _ = True
checkMove (PlacePiece (Piece x y ty col)) state = all (not . xyPieceEquals x y) state
checkMove (PlaceTerrain (Terrain x y ty)) state = all (not . xyTerrainEquals x y) state
checkMove (MovePiece (KingMove dir) (Piece x y King col)) state =
  canTraverseBoard (Piece x y King col) dir 1 state
checkMove (MovePiece (QueenMove dir dist) (Piece x y Queen col)) state =
  canTraverseBoard (Piece x y Queen col) dir dist state
checkMove (MovePiece (RookMove dir dist) (Piece x y Rook col)) state =
  checkRookDirection dir && checkCanTraverseBoard
    where
      checkCanTraverseBoard = canTraverseBoard (Piece x y Rook col) dir dist state
      checkRookDirection DirectionRight = True
      checkRookDirection DirectionLeft = True
      checkRookDirection DirectionUp = True
      checkRookDirection DirectionDown = True
      checkRookDirection _ = False
checkMove (MovePiece (BishopMove dir dist) (Piece x y Bishop col)) state =
  checkBishopDirection dir && checkCanTraverseBoard
    where
      checkCanTraverseBoard = canTraverseBoard (Piece x y Bishop col) dir dist state
      checkBishopDirection DirectionUpRight = True
      checkBishopDirection DirectionUpLeft = True
      checkBishopDirection DirectionDownRight = True
      checkBishopDirection DirectionDownLeft = True
      checkBishopDirection _ = False
checkMove (MovePiece (KnightMove kDir) (Piece x y Knight col)) state =
  (uncurry inBoardBounds) ((x, y) `vecAdd` (knightDirectionCoordinates kDir) )
    && (uncurry $ canEnterSquare state Knight DirectionUp) ((x, y)
      `vecAdd` (knightDirectionCoordinates kDir) )
checkMove (MovePiece PawnMove (Piece x y Pawn col)) state =
  canTraverseBoard (Piece x y Pawn col) (pawnColorDirection col) 1 state
checkMove _ _ = False
