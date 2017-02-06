// All the queries that have to go to the server

function doAction(m: GameMove) {
    let a = { tag: 'JsonAction', actor: 'bob', move: m };
    $.ajax({
        url: 'http://loka.jakespringer.com:8438/game/2/doaction',
        type: 'POST',
        data: JSON.stringify(a),
        success: processGameState
    });
}

function getState() {
    $.ajax({
        url: 'http://loka.jakespringer.com:8438/game/2/state',
        success: processGameState
    });

    pieceList = [{
        tag: 'Piece',
        pieceX: 5,
        pieceY: 2,
        pieceType: 'Rook',
        color: 'Red'
    }, {
        tag: 'Piece',
        pieceX: 1,
        pieceY: 2,
        pieceType: 'King',
        color: 'Blue'
    }, {
        tag: 'Piece',
        pieceX: 2,
        pieceY: 5,
        pieceType: 'Bishop',
        color: 'Green'
    }, {
        tag: 'Piece',
        pieceX: 7,
        pieceY: 4,
        pieceType: 'Knight',
        color: 'Yellow'
    }];
    //    pieceList = [new Piece(new Square(5, 0), 'rook', 'red'),
    //    new Piece(new Square(1, 2), 'king', 'blue'),
    //    new Piece(new Square(2, 5), 'bishop', 'green'),
    //    new Piece(new Square(7, 4), 'knight', 'yellow')];

}

function getMoves() {
    $.ajax({
        url: 'http://loka.jakespringer.com:8438/game/2/allmoves?color=Green',
        success: processMoveList
    })
}

// Process responses

function processGameState(response: JsonGameState) {
    pieceList = [];
    for (let pieceOrTerrain of response.static) {
        if (pieceOrTerrain.Left != undefined) {
            pieceList.push(pieceOrTerrain.Left);
        }
    }
    getMoves();
}

function processMoveList(response: GameMove[]) {
    moveList = [];
    for (let move of response) {
        if (move.tag === 'AttackPiece' || move.tag === 'MovePiece') {
            moveList.push(move);
        }
    }
    myTurn = true;
}

// Utility functions

function findMoveStartSquare(m: MovePiece | AttackPiece): Square {
    let p: Piece = m.tag === 'AttackPiece' ? m.attacker : m.piece;
    return new Square(p.pieceX, p.pieceY);
}
function findMoveTargetSquare(m: MovePiece | AttackPiece): Square {
    let p: Piece = m.tag === 'AttackPiece' ? m.attacker : m.piece;
    return new Square(p.pieceX, p.pieceY).add(pieceMoveToSquare(p, m.move));
}

function pieceMoveToSquare(p: Piece, move: PieceMove): Square {
    if (move.tag === 'PawnMove') return pawnDirectionToSquare(p.color);
    if (move.tag === 'KnightMove') return knightDirectionToSquare(move.knightDirection);
    if (move.tag === 'KingMove') return directionToSquare(move.direction);
    return directionToSquare(move.direction).multiply(move.distance);
}

function directionToSquare(direction: PieceDirection): Square {
    switch (direction) {
        case 'DirectionRight': return new Square(1, 0);
        case 'DirectionUpRight': return new Square(1, 1);
        case 'DirectionUp': return new Square(0, 1);
        case 'DirectionUpLeft': return new Square(-1, 1);
        case 'DirectionLeft': return new Square(-1, 0);
        case 'DirectionDownLeft': return new Square(-1, -1);
        case 'DirectionDown': return new Square(0, -1);
        case 'DirectionDownRight': return new Square(1, -1);
        default: return assertNever(direction);
    }
}

function knightDirectionToSquare(knightDirection: KnightDirection): Square {
    switch (knightDirection) {
        case 'KnightDirectionUpRight': return new Square(1, 2);
        case 'KnightDirectionUpLeft': return new Square(-1, 2);
        case 'KnightDirectionDownRight': return new Square(1, -2);
        case 'KnightDirectionDownLeft': return new Square(-1, -2);
        case 'KnightDirectionRightDown': return new Square(2, -1);
        case 'KnightDirectionRightUp': return new Square(2, 1);
        case 'KnightDirectionLeftDown': return new Square(-2, -1);
        case 'KnightDirectionLeftUp': return new Square(-2, 1);
        default: return assertNever(knightDirection);
    }
}

function pawnDirectionToSquare(color: PieceColor): Square {
    switch (color) {
        case 'Red': return new Square(0, 1);
        case 'Blue': return new Square(0, -1);
        case 'Green': return new Square(1, 0);
        case 'Yellow': return new Square(-1, 0);
        default: return assertNever(color);
    }
}

function assertNever(x: never): never {
    throw new Error('Unexpected object: ' + x);
}