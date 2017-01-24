// All the queries that have to go to the server

function doAction(a: ActionJSON) {
    $.ajax({
        url: "http://loka.jakespringer.com:8438/game/2/doaction",
        type: "POST",
        data: JSON.stringify(a),
        success: function(response: GetStateJSON) {
            pieceList = response.static.map(function(json) {
                return pieceJsonToPiece(json.Left);
            });
            getMoves();
        }
    });
}

function getState() {
    $.ajax({
        url: "http://loka.jakespringer.com:8438/game/2/state",
        success: function(response: GetStateJSON) {
            pieceList = response.static.map(function(json) {
                return pieceJsonToPiece(json.Left);
            });
        }
    });

    pieceList = [new Piece(new Square(5, 0), 'rook', 'red'),
    new Piece(new Square(1, 2), 'king', 'blue'),
    new Piece(new Square(2, 5), 'bishop', 'green'),
    new Piece(new Square(7, 4), 'knight', 'yellow')];

    getMoves();
}

function getMoves() {
    $.ajax({
        url: "http://loka.jakespringer.com:8438/game/2/allmoves?color=Green",
        success: function(response: MoveJSON[]) {
            moveList = response.map(moveJsonToMove);
            myTurn = true;
        }
    })
}

// Response interfaces

interface GetStateJSON {
    actions: ActionJSON[];
    static: {
        Left: PieceJSON;
    }[]
}

interface ActionJSON {
    actor: string;
    move: MoveJSON
}

interface PieceJSON {
    pieceX: number;
    pieceY: number;
    pieceType: string;
    color: string;
}

interface MoveJSON {
    piece: PieceJSON;
    tag: string;
    move?: {
        direction?: string;
        distance?: number;
        knightDirection?: string;
        tag: string;
    }
}

// Utility functions

function pieceJsonToPiece(pieceJson: PieceJSON): Piece {
    return new Piece(new Square(pieceJson.pieceX, pieceJson.pieceY), pieceJson.pieceType, pieceJson.color);
}

function moveJsonToMove(moveJson: MoveJSON): Move {
    let s1 = new Square(moveJson.piece.pieceX, moveJson.piece.pieceY);
    let d = new Square(0, 0);
    switch (moveJson.piece.pieceType) {
        case "Knight":
            d = knightDirectionToSquare(moveJson.move.knightDirection);
            break;
        case "Pawn":
            d = pawnDirectionToSquare(moveJson.piece.color);
            break;
        default:
            d = directionToSquare(moveJson.move.direction).multiply(moveJson.move.distance);
            break;
    }
    return new Move(s1, s1.add(d), moveJson);
}

function directionToSquare(direction: string): Square {
    switch (direction) {
        case "DirectionRight": return new Square(1, 0);
        case "DirectionUpRight": return new Square(1, 1);
        case "DirectionUp": return new Square(0, 1);
        case "DirectionUpLeft": return new Square(-1, 1);
        case "DirectionLeft": return new Square(-1, 0);
        case "DirectionDownLeft": return new Square(-1, -1);
        case "DirectionDown": return new Square(0, -1);
        case "DirectionDownRight": return new Square(1, -1);
    }
}

function knightDirectionToSquare(direction: string): Square {
    switch (direction) {
        case "KnightDirectionUpRight": return new Square(1, 2);
        case "KnightDirectionUpLeft": return new Square(-1, 2);
        case "KnightDirectionDownRight": return new Square(1, -2);
        case "KnightDirectionDownLeft": return new Square(-1, -2);
        case "KnightDirectionRightDown": return new Square(2, -1);
        case "KnightDirectionRightUp": return new Square(2, 1);
        case "KnightDirectionLeftDown": return new Square(-2, -1);
        case "KnightDirectionLeftUp": return new Square(-2, 1);
    }
}

function pawnDirectionToSquare(color: string): Square {
    switch (color) {
        case "Red": return new Square(0, 1);
        case "Blue": return new Square(0, -1);
        case "Green": return new Square(1, 0);
        case "Yellow": return new Square(-1, 0);
    }
}