{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-name-shadowing
    -fwarn-hi-shadowing -fno-warn-unused-matches
 #-}

module Jsonify where

import Data.List

import Loka
import Types

removeSpaces :: String -> String
removeSpaces str = filter ((/=) ' ') str

jsonifyPiece :: Piece -> String
jsonifyPiece (Piece x y color pieceType)= removeSpaces json
  where
    json = "{"
      ++   "\"x\": " ++ show x ++ ","
      ++   "\"y\": " ++ show y ++ ","
      ++   "\"color\": \"" ++ show color ++ "\","
      ++   "\"type\": \"" ++ show pieceType ++ "\""
      ++ "}"

jsonifyGameMove :: Actor -> GameMove -> Integer -> String
jsonifyGameMove actor Noop index = removeSpaces json
  where
    json = "{"
      ++   "\"actor\": \"" ++ actor ++ "\","
      ++   "\"actionIndex\": " ++ show index ++ ","
      ++   "\"type\": \"Noop\""
      ++ "}"

jsonifyGameMove actor (PlacePiece piece) index = removeSpaces json
  where
    json = "{"
      ++   "\"actor\": \"" ++ actor ++ "\","
      ++   "\"actionIndex\": " ++ show index ++ ","
      ++   "\"type\": \"PlacePiece\","
      ++   "\"piece\":" ++ jsonifyPiece piece
      ++ "}"

jsonifyGameMove actor (PlaceTerrain (Terrain x y (MountainPass direction))) index = removeSpaces json
  where
    json = "{"
      ++   "\"actor\": \"" ++ actor ++ "\","
      ++   "\"actionIndex\": " ++ show index ++ ","
      ++   "\"type\": \"PlaceTerrain\","
      ++   "\"terrain\": {"
      ++     "\"x\": \"" ++ show x ++ "\","
      ++     "\"y\": \"" ++ show y ++ "\","
      ++     "\"type\": \"MountainPass\","
      ++     "\"direction\": \"" ++ show direction ++ "\""
      ++   "}"
      ++ "}"

jsonifyGameMove actor (PlaceTerrain (Terrain x y terrainType)) index = removeSpaces json
  where
    json = "{"
      ++   "\"actor\": \"" ++ actor ++ "\","
      ++   "\"actionIndex\": " ++ show index ++ ","
      ++   "\"type\": \"PlaceTerrain\","
      ++   "\"terrain\": {"
      ++     "\"x\": \"" ++ show x ++ "\","
      ++     "\"y\": \"" ++ show y ++ "\","
      ++     "\"type\": \"" ++ show terrainType ++ "\""
      ++   "}"
      ++ "}"

jsonifyGameMove actor (MovePiece (KingMove direction) (Piece x y King color)) index = removeSpaces json
  where
    json = "{"
      ++   "\"actor\": \"" ++ actor ++ "\","
      ++   "\"actionIndex\": " ++ show index ++ ","
      ++   "\"type\": \"MovePiece\","
      ++   "\"subtype\": \"KingMove\","
      ++   "\"piece\":" ++ jsonifyPiece (Piece x y King color) ++ ","
      ++   "\"direction\": " ++ show direction
      ++ "}"

jsonifyGameMove actor (MovePiece (QueenMove direction distance) (Piece x y Queen color)) index = removeSpaces json
  where
    json = "{"
      ++   "\"actor\": \"" ++ actor ++ "\","
      ++   "\"actionIndex\": " ++ show index ++ ","
      ++   "\"type\": \"MovePiece\","
      ++   "\"subtype\": \"QueenMove\","
      ++   "\"piece\":" ++ jsonifyPiece (Piece x y Queen color) ++ ","
      ++   "\"direction\": \"" ++ show direction ++ "\","
      ++   "\"distance\": " ++ show distance
      ++ "}"

jsonifyGameMove actor (MovePiece (RookMove direction distance) (Piece x y Rook color)) index = removeSpaces json
  where
    json = "{"
      ++   "\"actor\": \"" ++ actor ++ "\","
      ++   "\"actionIndex\": " ++ show index ++ ","
      ++   "\"type\": \"MovePiece\","
      ++   "\"subtype\": \"RookMove\","
      ++   "\"piece\":" ++ jsonifyPiece (Piece x y Rook color) ++ ","
      ++   "\"direction\": \"" ++ show direction ++ "\","
      ++   "\"distance\": " ++ show distance
      ++ "}"

jsonifyGameMove actor (MovePiece (BishopMove direction distance) (Piece x y Bishop color)) index = removeSpaces json
  where
    json = "{"
      ++   "\"actor\": \"" ++ actor ++ "\","
      ++   "\"actionIndex\": " ++ show index ++ ","
      ++   "\"type\": \"MovePiece\","
      ++   "\"subtype\": \"BishopMove\","
      ++   "\"piece\":" ++ jsonifyPiece (Piece x y Bishop color) ++ ","
      ++   "\"direction\": \"" ++ show direction ++ "\","
      ++   "\"distance\": " ++ show distance
      ++ "}"

jsonifyGameMove actor (MovePiece (KnightMove knightDirection) (Piece x y Knight color)) index = removeSpaces json
  where
    json = "{"
      ++   "\"actor\": \"" ++ actor ++ "\","
      ++   "\"actionIndex\": " ++ show index ++ ","
      ++   "\"type\": \"MovePiece\","
      ++   "\"subtype\": \"KnightMove\","
      ++   "\"piece\":" ++ jsonifyPiece (Piece x y Knight color) ++ ","
      ++   "\"direction\": \"" ++ show knightDirection ++ "\","
      ++ "}"

jsonifyGameMove actor (MovePiece PawnMove (Piece x y Pawn color)) index = removeSpaces json
  where
    json = "{"
      ++   "\"actor\": \"" ++ actor ++ "\","
      ++   "\"actionIndex\": " ++ show index ++ ","
      ++   "\"type\": \"MovePiece\","
      ++   "\"subtype\": \"PawnMove\","
      ++   "\"piece\":" ++ jsonifyPiece (Piece x y Pawn color)
      ++ "}"

jsonifyGameMove _ _ _ = "{}"

jsonifyValidGameState :: GameState -> String
jsonifyValidGameState state = removeSpaces json
  where
    json = "{"
      ++   "\"status\" : \"success\","
      ++   "\"actions\" : ["
      ++     (concat $ intersperse "," $ map jsonifyGameMoveHelper $ zip state [0..(length state - 1)])
      ++   "]"
      ++ "}"
    jsonifyGameMoveHelper ((actor, move), index) = jsonifyGameMove actor move $ fromIntegral index

jsonifyInvalidGameState :: String -> String
jsonifyInvalidGameState errorMessage = removeSpaces json
  where
    json = "{"
      ++   "\"status\" : \"error\","
      ++   "\"message\" : \"" ++ errorMessage ++ "\""
      ++ "}"
