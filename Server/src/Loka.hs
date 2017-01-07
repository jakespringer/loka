module Loka.Core where

import Data.Maybe

data Roll = Roll Integer

data PieceDirection = DirectionRight | DirectionUpRight | DirectionUp | DirectionUpLeft | DirectionLeft | DirectionDownLeft | DirectionDown | DirectionDownRight

data KnightDirection = KnightDirectionUpRight | KnightDirectionUpLeft
                     | KnightDirectionRightUp | KnightDirectionRightDown
                     | KnightDirectionLeftUp | KnightDirectionLeftDown
                     | KnightDirectionDownRight | KnightDirectionDownLeft

data MountainPassDirection = MountainPassDirectionHorizontal | MountainPassDirectionVertical

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn

data PieceColor = Red | Green | Blue | Yellow

data PieceMove = KingMove { direction :: PieceDirection }
               | QueenMove { direction :: PieceDirection, distance :: Integer }
               | RookMove { direction :: PieceDirection, distance :: Integer }
               | BishopMove { direction :: PieceDirection, distance :: Integer }
               | KnightMove { knightDirection :: KnightDirection }
               | PawnMove

data Piece = Piece { pieceX :: Integer
                   , pieceY :: Integer
                   , pieceType :: PieceType
                   , color :: PieceColor }

data TerrainType = Eyrie | Castle | Swamp | MountainPass MountainPassDirection | Forest | Lake | StoneCircle | Portal | None

data Terrain = Terrain { terrainX :: Integer
                       , terrainY :: Integer
                       , terrainType :: TerrainType }

data GameMove = Noop
              | PlacePiece { piece :: Piece }
              | PlaceTerrain { terrain :: Terrain }
              | MovePiece { move :: PieceMove, piece :: Piece }
              | AttackPiece { move :: PieceMove
                            , attacker :: Piece
                            , defender :: Piece
                            , attackerRoll :: Roll
                            , defenderRoll :: Roll }
              | MultiMove [GameMove]

type GameState = [GameMove]

type StaticSquare = (Maybe (PieceType, PieceColor), TerrainType)
type StaticGameState = [StaticSquare]

-- Helper functions, not exported

boardSize :: Integer
boardSize = 12

cornerSize :: Integer
cornerSize = 3

index2d :: Integer -> Integer -> Integer -> Int
index2d x y width = fromIntegral $ y * width + x

indexPiece :: Piece -> Int
indexPiece piece = index2d (pieceX piece) (pieceY piece) boardSize

indexTerrain :: Terrain -> Int
indexTerrain terrain = index2d (terrainX terrain) (terrainY terrain) boardSize

inBoardBounds :: Integer -> Integer -> Bool
inBoardBounds x y = 0 <= index && index < (boardSize * boardSize)
  where index = toInteger $ index2d x y boardSize

gameElementAt :: StaticGameState -> Integer -> Integer -> StaticSquare
gameElementAt state x y = state !! index2d x y boardSize

pieceAt :: StaticGameState -> Integer -> Integer -> Maybe (PieceType, PieceColor)
pieceAt state x y = case gameElementAt state x y of
  (piece, _) -> piece

terrainAt :: StaticGameState -> Integer -> Integer -> TerrainType
terrainAt state x y = snd $ gameElementAt state x y

maybeElem :: [a] -> Int -> Maybe a
maybeElem arr index = if 0 < index && index < length arr
  then Just (arr !! index)
  else Nothing

tupleMap :: (a -> b) -> (a, a) -> (b, b)
tupleMap f (x, y) = (f x, f y)

toCoordinates :: PieceDirection -> (Integer, Integer)
toCoordinates DirectionRight = (1, 0)
toCoordinates DirectionUpRight = (1, 1)
toCoordinates DirectionUp = (0, 1)
toCoordinates DirectionUpLeft = (-1, 1)
toCoordinates DirectionLeft = (-1, 0)
toCoordinates DirectionDownLeft = (-1, -1)
toCoordinates DirectionDown = (0, -1)
toCoordinates DirectionDownRight = (1, -1)

-- KnightDirectionUpRight | KnightDirectionUpLeft
--                      | KnightDirectionRightRight | KnightDirectionRightLeft
--                      | KnightDirectionLeftRight | KnightDirectionLeftLeft
--                      | KnightDirectionDownRight | KnightDirectionDownLeft

toKnightCoordinates :: KnightDirection -> (Integer, Integer)
toKnightCoordinates KnightDirectionUpRight = (1, 2)
toKnightCoordinates KnightDirectionUpLeft = (-1, 2)
toKnightCoordinates KnightDirectionDownRight = (1, -2)
toKnightCoordinates KnightDirectionDownLeft = (-1, -2)
toKnightCoordinates KnightDirectionRightDown = (2, -1)
toKnightCoordinates KnightDirectionRightUp = (2, 1)
toKnightCoordinates KnightDirectionLeftDown = (-2, -1)
toKnightCoordinates KnightDirectionLeftUp = (-2, 1)

pawnColorToDirection :: PieceColor -> PieceDirection
pawnColorToDirection Red = DirectionUp
pawnColorToDirection Blue = DirectionDown
pawnColorToDirection Green = DirectionRight
pawnColorToDirection Yellow = DirectionLeft

genStraightLineList :: PieceDirection -> Integer -> (Integer, Integer) -> [(Integer, Integer)]
genStraightLineList direction distance (x, y) = map (lastSquare direction x y) [1..distance]
  where
    lastSquare direction x y distance = (x + dx, y + dy)
      where
        (dx, dy) = tupleMap (*distance) $ toCoordinates direction

zipStraightLineOffsetList :: PieceDirection -> Integer -> (Integer, Integer) -> [((Integer, Integer), Maybe (Integer, Integer))]
zipStraightLineOffsetList direction distance (x, y) = ((x, y), Nothing):(zip (gen x y) (genMaybe (x-dx) (y-dy)))
  where
    (dx, dy) = toCoordinates direction
    gen x y = genStraightLineList direction distance (x, y)
    genMaybe x y = map (\x -> Just x) $ gen x y

canTraverseBoard :: StaticGameState -> Piece -> PieceDirection -> Integer -> Bool
canTraverseBoard state (Piece x y pieceType _) direction distance =
  all canEnterSquare $ zipStraightLineOffsetList direction distance (x, y)
    where
      canEnterSquare ((squareX, squareY), Just (previousSquareX, previousSquareY)) =
        currentlyInBoardBounds && canEnterCurrentSquare && canEnterCurrentTerrain
          where
            currentlyInBoardBounds = inBoardBounds squareX squareY
            currentTerrainType = terrainAt state squareX squareY
            previousTerrainType = terrainAt state previousSquareX previousSquareY
            canEnterCurrentTerrain = canEnterTerrain currentTerrainType pieceType direction (Just previousTerrainType)
            canEnterCurrentSquare = isNothing $ pieceAt state squareX squareY

-- canEnter :: terrainToEnter -> pieceType -> incomingDirection -> previousSquare
canEnterTerrain :: TerrainType -> PieceType -> PieceDirection -> Maybe TerrainType -> Bool
canEnterTerrain _ _ _ (Just Swamp) = False
canEnterTerrain Eyrie Knight _ _ = True
canEnterTerrain Eyrie _ _ _ = False
canEnterTerrain Castle _ _ _ = True
canEnterTerrain Swamp _ _ _ = True
canEnterTerrain (MountainPass _) Knight _ _ = True
canEnterTerrain (MountainPass MountainPassDirectionVertical) _ DirectionUp _ = True
canEnterTerrain (MountainPass MountainPassDirectionVertical) _ DirectionDown _ = True
canEnterTerrain (MountainPass MountainPassDirectionHorizontal) _ DirectionRight _ = True
canEnterTerrain (MountainPass MountainPassDirectionHorizontal) _ DirectionLeft _ = True
canEnterTerrain (MountainPass _) _ _ _ = False
canEnterTerrain Forest Pawn _ _ = True
canEnterTerrain Forest _ _ _ = False
canEnterTerrain Lake _ _ _ = False
canEnterTerrain StoneCircle Rook _ _ = True
canEnterTerrain StoneCircle _ _ _  = False
canEnterTerrain Portal _ _ _ = True
canEnterTerrain None _ _ _ = True

-- Noop
-- | PlacePiece { piece :: Piece }
-- | PlaceTerrain { terrain :: Terrain }
-- | MovePiece { piece :: Piece, move :: PieceMove }
-- | AttackPiece { attacker :: Piece
--               , defender :: Piece
--               , move :: PieceMove
--               , attackerRoll :: Roll
--               , defenderRoll :: Roll }
-- | MultiMove [GameMove]

checkValidMove :: StaticGameState -> GameMove -> Bool
checkValidMove _ Noop = True

checkValidMove state (PlacePiece piece) = case gameElementAt state (pieceX piece) (pieceY piece) of
  (Nothing, _) -> True
  _ -> False

checkValidMove state (PlaceTerrain (Terrain x y _)) = case gameElementAt state x y of
  (_, None) -> True
  _ -> False

checkValidMove state (MovePiece (KingMove direction) (Piece x y King color)) =
  canTraverseBoard state (Piece x y King color) direction 1

checkValidMove state (MovePiece (QueenMove direction distance) (Piece x y Queen color)) =
  canTraverseBoard state (Piece x y Queen color) direction distance

checkValidMove state (MovePiece (RookMove direction distance) (Piece x y Rook color)) =
  checkRookDirection direction && checkCanTraverseBoard
    where
      checkCanTraverseBoard = canTraverseBoard state (Piece x y Rook color) direction distance
      checkRookDirection DirectionRight = True
      checkRookDirection DirectionLeft = True
      checkRookDirection DirectionUp = True
      checkRookDirection DirectionDown = True
      checkRookDirection _ = False

checkValidMove state (MovePiece (BishopMove direction distance) (Piece x y Bishop color)) =
  checkBishopDirection direction && checkCanTraverseBoard
    where
      checkCanTraverseBoard = canTraverseBoard state (Piece x y Bishop color) direction distance
      checkBishopDirection DirectionUpRight = True
      checkBishopDirection DirectionUpLeft = True
      checkBishopDirection DirectionDownRight = True
      checkBishopDirection DirectionDownLeft = True
      checkBishopDirection _ = False

checkValidMove state (MovePiece (KnightMove knightDirection) (Piece x y Knight color)) =
  (inBoardBounds x y) && (canEnterTerrain (terrainAt state x y) Knight DirectionUp Nothing)

checkValidMove state (MovePiece PawnMove (Piece x y Pawn color)) =
  canTraverseBoard state (Piece x y Pawn color) (pawnColorToDirection color) 1

checkValidMove _ _ = False
-- End helper function
