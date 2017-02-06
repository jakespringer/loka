
type Roll = string;

type PieceDirection = 'DirectionRight' | 'DirectionUpRight' | 'DirectionUp' | 'DirectionUpLeft'
    | 'DirectionLeft' | 'DirectionDownLeft' | 'DirectionDown' | 'DirectionDownRight'
type KnightDirection = 'KnightDirectionUpRight' | 'KnightDirectionUpLeft'
    | 'KnightDirectionRightUp' | 'KnightDirectionRightDown'
    | 'KnightDirectionLeftUp' | 'KnightDirectionLeftDown'
    | 'KnightDirectionDownRight' | 'KnightDirectionDownLeft'
type MountainPassDirection = 'MountainPassDirectionHorizontal' | 'MountainPassDirectionVertical'

type PieceType = 'King' | 'Queen' | 'Rook' | 'Bishop' | 'Knight' | 'Pawn'
type PieceColor = 'Red' | 'Green' | 'Blue' | 'Yellow'

interface KingMove { tag: 'KingMove'; direction: PieceDirection; }
interface QueenMove { tag: 'QueenMove'; direction: PieceDirection; distance: number; }
interface RookMove { tag: 'RookMove'; direction: PieceDirection; distance: number; }
interface BishopMove { tag: 'BishopMove'; direction: PieceDirection; distance: number; }
interface KnightMove { tag: 'KnightMove'; knightDirection: KnightDirection; }
interface PawnMove { tag: 'PawnMove'; }
type PieceMove = KingMove | QueenMove | RookMove | BishopMove | KnightMove | PawnMove

interface Piece {
    tag: 'Piece';
    pieceX: number;
    pieceY: number;
    pieceType: PieceType;
    color: PieceColor;
}

type TerrainType = ['MountainPass', MountainPassDirection] | 'Eyrie'
    | 'Castle' | 'Swamp' | 'Forest' | 'Lake' | 'StoneCircle' | 'Portal'
interface Terrain {
    tag: 'Terrain';
    terrainX: number;
    terrainY: number;
    terrainType: TerrainType;
}

interface PlacePiece { tag: 'PlacePiece'; piece: Piece; }
interface PlaceTerrain { tag: 'PlaceTerrain'; terrain: Terrain; }
interface MovePiece { tag: 'MovePiece'; move: PieceMove; piece: Piece; }
interface AttackPiece {
    tag: 'AttackPiece';
    move: PieceMove;
    attacker: Piece;
    defender: Piece;
    attackerRoll: Roll;
    defenderRoll: Roll;
}
type GameMove = { tag: 'Noop' } | PlacePiece | PlaceTerrain | MovePiece | AttackPiece

type CollapsedGameState = { Left?: Piece; Right?: Terrain; }[]


interface JsonAction {
    tag: 'JsonAction';
    actor: string;
    move: GameMove;
}

interface JsonGameState {
    tag: 'JsonGameState';
    actions: JsonAction[];
    static: CollapsedGameState;
}