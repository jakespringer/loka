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
var Piece = (function () {
    function Piece(square, pieceType, team) {
        this.square = square;
        this.pieceType = pieceType;
        this.team = team;
    }
    Piece.prototype.toString = function () {
        return 'Piece{' + this.square + ',' + this.pieceType + '' + this.team + '}';
    };
    return Piece;
}());
var Square = (function () {
    function Square(x, y, terrain) {
        this.x = x;
        this.y = y;
        this.terrain = terrain;
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
var Move = (function () {
    function Move(from, to, json) {
        this.from = from;
        this.to = to;
        this.json = json;
    }
    Move.prototype.equals = function (other) {
        return this.from.equals(other.from) && this.to.equals(other.to);
    };
    Move.prototype.toString = function () {
        return 'Move{' + this.from + ',' + this.to + '}';
    };
    return Move;
}());
var AttackResult = (function () {
    function AttackResult(attackerDie, defenderDie) {
        this.attackerDie = attackerDie;
        this.defenderDie = defenderDie;
    }
    return AttackResult;
}());
var Die = (function () {
    function Die(sides, roll) {
        this.sides = sides;
        this.roll = roll;
    }
    return Die;
}());
var sprites = {
    red: load_colored_images(new Color(.7, 0, 0)),
    green: load_colored_images(new Color(.3, 1, .3)),
    blue: load_colored_images(new Color(0, 0, .7)),
    yellow: load_colored_images(new Color(1, 1, 0))
};
function load_colored_images(color) {
    var obj = {};
    var _loop_1 = function (piece) {
        var image = new Image();
        image.src = 'img/' + piece + '_white.png';
        image.onload = function () {
            image.onload = function () { };
            tintImage(image, color);
            //            image
            //            ctx.save();
            //            ctx.drawImage(image, 0, 0);
            //            ctx.globalCompositeOperation = 'multiply';
            //            ctx.fillStyle = color;
            //            ctx.fillRect(0, 0, canvas.width, canvas.height);
            //            ctx.restore();
        };
        obj[piece] = image;
    };
    for (var _i = 0, _a = ['bishop', 'king', 'knight', 'pawn', 'queen', 'rook']; _i < _a.length; _i++) {
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
var canvas = document.createElement("canvas");
document.body.appendChild(canvas);
var ctx = canvas.getContext("2d");
// The game state
var moveList = [];
var pieceList = [];
var board = [];
var boardSize = 12;
for (var x = 0; x < boardSize; x++) {
    board.push([]);
    for (var y = 0; y < boardSize; y++) {
        board[x].push(new Square(x, y));
    }
}
var squareSize = getSquareSize();
var selected = null;
var selectedOffset = { x: 0, y: 0 };
var mousePos = { x: 0, y: 0 };
var myTurn = false;
getState();
// Responding to user input
canvas.addEventListener("mousedown", function (e) {
    if (myTurn && selected == null) {
        var s = getSquare(e.clientX, e.clientY);
        if (getPieceOnSquare(s) != null) {
            selected = getPieceOnSquare(s);
            selectedOffset = { x: e.clientX - s.getX(), y: e.clientY - s.getY() };
        }
    }
});
canvas.addEventListener("mousemove", function (e) {
    mousePos = { x: e.clientX, y: e.clientY };
});
canvas.addEventListener("mouseup", function (e) {
    if (selected != null) {
        var s = getSquare(e.clientX, e.clientY);
        for (var _i = 0, moveList_1 = moveList; _i < moveList_1.length; _i++) {
            var m = moveList_1[_i];
            if (m.equals(new Move(selected.square, s))) {
                myTurn = false;
                doAction({ actor: "bob", move: m.json });
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
            var c = (x % 2 + y % 2 == 1) ? new Color(.9, .9, .9) : new Color(.2, .2, .2);
            ctx.fillStyle = c.toFillStyle();
            ctx.fillRect(x * squareSize, y * squareSize, squareSize, squareSize);
            ctx.fillStyle = "red";
            ctx.fillText(x + ' ' + y, x * squareSize, (y + 1) * squareSize);
        }
    }
    for (var _i = 0, pieceList_1 = pieceList; _i < pieceList_1.length; _i++) {
        var p = pieceList_1[_i];
        if (p != selected) {
            ctx.drawImage(sprites[p.team][p.pieceType], p.square.getX(), p.square.getY(), squareSize, squareSize);
        }
    }
    if (selected != null) {
        for (var _a = 0, moveList_2 = moveList; _a < moveList_2.length; _a++) {
            var m = moveList_2[_a];
            if (m.from.equals(selected.square)) {
                ctx.fillStyle = new Color(1, 1, 0, .5).toFillStyle();
                ctx.fillRect(m.to.x * squareSize, m.to.y * squareSize, squareSize, squareSize);
            }
        }
        ctx.drawImage(sprites[selected.team][selected.pieceType], mousePos.x - selectedOffset.x, mousePos.y - selectedOffset.y, squareSize, squareSize);
    }
}
// Misc utility functions
function getSquare(x, y) {
    if (Math.min(x, y) < 0 || Math.max(x, y) >= squareSize * boardSize) {
        return null;
    }
    return board[Math.floor(x / squareSize)][Math.floor(y / squareSize)];
}
function getSquareSize() {
    return Math.min(window.innerWidth, window.innerHeight) / 12;
}
function getPieceOnSquare(s) {
    for (var _i = 0, pieceList_2 = pieceList; _i < pieceList_2.length; _i++) {
        var p = pieceList_2[_i];
        if (p.square == s) {
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
function doAction(a) {
    $.ajax({
        url: "http://loka.jakespringer.com:8438/game/2/doaction",
        type: "POST",
        data: JSON.stringify(a),
        success: function (response) {
            pieceList = response.static.map(function (json) {
                return pieceJsonToPiece(json.Left);
            });
            getMoves();
        }
    });
}
function getState() {
    $.ajax({
        url: "http://loka.jakespringer.com:8438/game/2/state",
        success: function (response) {
            pieceList = response.static.map(function (json) {
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
        success: function (response) {
            moveList = response.map(moveJsonToMove);
            myTurn = true;
        }
    });
}
// Utility functions
function pieceJsonToPiece(pieceJson) {
    return new Piece(new Square(pieceJson.pieceX, pieceJson.pieceY), pieceJson.pieceType, pieceJson.color);
}
function moveJsonToMove(moveJson) {
    var s1 = new Square(moveJson.piece.pieceX, moveJson.piece.pieceY);
    var d = new Square(0, 0);
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
function directionToSquare(direction) {
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
function knightDirectionToSquare(direction) {
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
function pawnDirectionToSquare(color) {
    switch (color) {
        case "Red": return new Square(0, 1);
        case "Blue": return new Square(0, -1);
        case "Green": return new Square(1, 0);
        case "Yellow": return new Square(-1, 0);
    }
}
//# sourceMappingURL=loka.js.map