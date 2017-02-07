// All the classes used to store data
var Color = (function () {
    function Color(r, g, b, a) {
        if (a === void 0) { a = 1; }
        this.r = r;
        this.g = g;
        this.b = b;
        this.a = a;
    }
    Color.prototype.toDrawingColor = function () {
        var f = function (x) { return Math.floor((x > 1 ? 1 : x) * 255); };
        return { r: f(this.r), g: f(this.g), b: f(this.b), a: this.a };
    };
    Color.prototype.toFillStyle = function () {
        var c = this.toDrawingColor();
        return 'rgba(' + c.r + ', ' + c.g + ', ' + c.b + ', ' + c.a + ')';
    };
    Color.prototype.toString = function () {
        return 'Color{' + this.r + ',' + this.g + ',' + this.b + ',' + this.a + '}';
    };
    return Color;
}());
var Square = (function () {
    function Square(x, y) {
        this.x = x;
        this.y = y;
    }
    Square.prototype.add = function (other) {
        return new Square(this.x + other.x, this.y + other.y);
    };
    Square.prototype.equals = function (other) {
        return other != null && this.x == other.x && this.y == other.y;
    };
    Square.prototype.getX = function () {
        return this.x * squareSize;
    };
    Square.prototype.getY = function () {
        return this.y * squareSize;
    };
    Square.prototype.multiply = function (c) {
        return new Square(this.x * c, this.y * c);
    };
    Square.prototype.toString = function () {
        return 'Square{' + this.x + ',' + this.y + '}';
    };
    return Square;
}());
var sprites = {
    Red: loadColoredImages(new Color(.7, 0, 0)),
    Green: loadColoredImages(new Color(.6, 1, .6)),
    Blue: loadColoredImages(new Color(.2, .2, .7)),
    Yellow: loadColoredImages(new Color(1, 1, .5))
};
function loadColoredImages(color) {
    var obj = {};
    var _loop_1 = function (piece) {
        var image = new Image();
        image.src = 'img/' + piece + '_white.png';
        image.onload = function () {
            image.onload = function () { };
            tintImage(image, color);
        };
        obj[piece] = image;
    };
    for (var _i = 0, _a = ['Bishop', 'King', 'Knight', 'Pawn', 'Queen', 'Rook']; _i < _a.length; _i++) {
        var piece = _a[_i];
        _loop_1(piece);
    }
    return obj;
}
function tintImage(imgElement, color) {
    // create hidden canvas (using image dimensions)
    var canvas = document.createElement('canvas');
    canvas.width = imgElement.width;
    canvas.height = imgElement.height;
    var ctx = canvas.getContext('2d');
    ctx.drawImage(imgElement, 0, 0);
    var imageData = ctx.getImageData(0, 0, canvas.width, canvas.height);
    var data = imageData.data;
    for (var p = 0; p < data.length; p += 4) {
        data[p] *= color.r;
        data[p + 1] *= color.g;
        data[p + 2] *= color.b;
    }
    ctx.putImageData(imageData, 0, 0);
    imgElement.src = canvas.toDataURL();
}
///<reference path='images.ts' />
var canvas = document.createElement('canvas');
document.body.appendChild(canvas);
var ctx = canvas.getContext('2d');
// The game state
var pieceList = [];
var terrainList = [];
var moveList = [];
var boardSize = 12;
var squareSize = getSquareSize();
var selected = null;
var selectedOffset = { x: 0, y: 0 };
var mousePos = { x: 0, y: 0 };
var myTurn = false;
getState();
// Responding to user input
canvas.addEventListener('mousedown', function (e) {
    if (myTurn && selected == null) {
        var s = getSquare(e.clientX, e.clientY);
        if (getPieceOnSquare(s) != null) {
            selected = getPieceOnSquare(s);
            selectedOffset = { x: e.clientX - s.getX(), y: e.clientY - s.getY() };
        }
    }
});
canvas.addEventListener('mousemove', function (e) {
    mousePos = { x: e.clientX, y: e.clientY };
});
canvas.addEventListener('mouseup', function (e) {
    if (selected != null) {
        var s = getSquare(e.clientX, e.clientY);
        for (var _i = 0, moveList_1 = moveList; _i < moveList_1.length; _i++) {
            var m = moveList_1[_i];
            if (findMoveStartSquare(m).equals(new Square(selected.pieceX, selected.pieceY))
                && findMoveTargetSquare(m).equals(s)) {
                myTurn = false;
                doAction(m);
                break;
            }
        }
        selected = null;
    }
});
// The draw function
function draw() {
    var width = ctx.canvas.width = window.innerWidth;
    var height = ctx.canvas.height = window.innerHeight;
    ctx.clearRect(0, 0, width, height);
    squareSize = getSquareSize();
    for (var x = 0; x < boardSize; x++) {
        for (var y = 0; y < boardSize; y++) {
            if (x >= 3 && x < 9 || y >= 3 && y < 9) {
                var c = (x % 2 + y % 2 == 1) ? new Color(.9, .9, .9) : new Color(.2, .2, .2);
                ctx.fillStyle = c.toFillStyle();
                ctx.fillRect(x * squareSize, y * squareSize, squareSize, squareSize);
                ctx.fillStyle = 'red';
                ctx.fillText(x + ' ' + y, x * squareSize, (y + 1) * squareSize);
            }
            else {
                ctx.fillStyle = 'black';
                ctx.fillRect(x * squareSize, y * squareSize, squareSize, squareSize);
            }
        }
    }
    for (var _i = 0, pieceList_1 = pieceList; _i < pieceList_1.length; _i++) {
        var p = pieceList_1[_i];
        if (p != selected) {
            ctx.drawImage(sprites[p.color][p.pieceType], p.pieceX * squareSize, p.pieceY * squareSize, squareSize, squareSize);
        }
    }
    if (selected != null) {
        for (var _a = 0, moveList_2 = moveList; _a < moveList_2.length; _a++) {
            var m = moveList_2[_a];
            if (findMoveStartSquare(m).equals(new Square(selected.pieceX, selected.pieceY))) {
                ctx.fillStyle = new Color(1, 1, 0, .5).toFillStyle();
                ctx.fillRect(findMoveTargetSquare(m).getX(), findMoveTargetSquare(m).getY(), squareSize, squareSize);
            }
        }
        ctx.drawImage(sprites[selected.color][selected.pieceType], mousePos.x - selectedOffset.x, mousePos.y - selectedOffset.y, squareSize, squareSize);
    }
    ctx.fillStyle = 'red';
    ctx.fillRect(mousePos.x - 10, mousePos.y - 10, 10, 10);
}
// Misc utility functions
function getSquare(x, y) {
    if (Math.min(x, y) < 0 || Math.max(x, y) >= squareSize * boardSize) {
        return null;
    }
    return new Square(Math.floor(x / squareSize), Math.floor(y / squareSize));
}
function getSquareSize() {
    return Math.min(canvas.width, canvas.height) / boardSize;
}
function getPieceOnSquare(s) {
    for (var _i = 0, pieceList_2 = pieceList; _i < pieceList_2.length; _i++) {
        var p = pieceList_2[_i];
        if (new Square(p.pieceX, p.pieceY).equals(s)) {
            return p;
        }
    }
    return null;
}
// The main loop
function mainLoop() {
    draw();
    window.requestAnimationFrame(mainLoop);
}
mainLoop();
// All the queries that have to go to the server
function doAction(m) {
    var a = { tag: 'JsonAction', actor: 'bob', move: m };
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
    });
}
// Process responses
function processGameState(response) {
    pieceList = [];
    for (var _i = 0, _a = response.static; _i < _a.length; _i++) {
        var pieceOrTerrain = _a[_i];
        if (pieceOrTerrain.Left != undefined) {
            pieceList.push(pieceOrTerrain.Left);
        }
    }
    getMoves();
}
function processMoveList(response) {
    moveList = [];
    for (var _i = 0, response_1 = response; _i < response_1.length; _i++) {
        var move = response_1[_i];
        if (move.tag === 'AttackPiece' || move.tag === 'MovePiece') {
            moveList.push(move);
        }
    }
    myTurn = true;
}
// Utility functions
function findMoveStartSquare(m) {
    var p = m.tag === 'AttackPiece' ? m.attacker : m.piece;
    return new Square(p.pieceX, p.pieceY);
}
function findMoveTargetSquare(m) {
    var p = m.tag === 'AttackPiece' ? m.attacker : m.piece;
    return new Square(p.pieceX, p.pieceY).add(pieceMoveToSquare(p, m.move));
}
function pieceMoveToSquare(p, move) {
    if (move.tag === 'PawnMove')
        return pawnDirectionToSquare(p.color);
    if (move.tag === 'KnightMove')
        return knightDirectionToSquare(move.knightDirection);
    if (move.tag === 'KingMove')
        return directionToSquare(move.direction);
    return directionToSquare(move.direction).multiply(move.distance);
}
function directionToSquare(direction) {
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
function knightDirectionToSquare(knightDirection) {
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
function pawnDirectionToSquare(color) {
    switch (color) {
        case 'Red': return new Square(0, 1);
        case 'Blue': return new Square(0, -1);
        case 'Green': return new Square(1, 0);
        case 'Yellow': return new Square(-1, 0);
        default: return assertNever(color);
    }
}
function assertNever(x) {
    throw new Error('Unexpected object: ' + x);
}
//# sourceMappingURL=loka.js.map