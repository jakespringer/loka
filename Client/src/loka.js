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
        this.image = new Image();
        this.image.src = 'img/' + pieceType + '_' + team + '.png';
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
    Square.prototype.equals = function (other) {
        return other != null && this.x == other.x && this.y == other.y;
    };
    Square.prototype.getX = function () {
        return this.x * squareSize;
    };
    Square.prototype.getY = function () {
        return this.y * squareSize;
    };
    Square.prototype.toString = function () {
        return 'Square{' + this.x + ',' + this.y + '}';
    };
    return Square;
}());
var Move = (function () {
    function Move(from, to) {
        this.from = from;
        this.to = to;
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
        var s = getSquare(e.x, e.y);
        if (getPieceOnSquare(s) != null) {
            selected = getPieceOnSquare(s);
            selectedOffset = { x: e.x - s.getX(), y: e.y - s.getY() };
        }
    }
});
canvas.addEventListener("mousemove", function (e) {
    mousePos = { x: e.x, y: e.y };
});
canvas.addEventListener("mouseup", function (e) {
    if (selected != null) {
        var s_1 = getSquare(e.x, e.y);
        if (moveList.some(function (m) { return m.equals(new Move(selected.square, s_1)); })) {
            myTurn = false;
            doMove(new Move(selected.square, s_1));
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
        }
    }
    for (var _i = 0, pieceList_1 = pieceList; _i < pieceList_1.length; _i++) {
        var p = pieceList_1[_i];
        if (p != selected) {
            ctx.drawImage(p.image, p.square.getX(), p.square.getY(), squareSize, squareSize);
        }
    }
    if (selected != null) {
        for (var _a = 0, moveList_1 = moveList; _a < moveList_1.length; _a++) {
            var m = moveList_1[_a];
            if (m.from.equals(selected.square)) {
                ctx.fillStyle = new Color(1, 1, 0, .5).toFillStyle();
                ctx.fillRect(m.to.x * squareSize, m.to.y * squareSize, squareSize, squareSize);
            }
        }
        ctx.drawImage(selected.image, mousePos.x - selectedOffset.x, mousePos.y - selectedOffset.y, squareSize, squareSize);
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
function updateState(result) {
    moveList = result.moveList;
    pieceList = result.pieceList;
    for (var _i = 0, _a = result.terrain; _i < _a.length; _i++) {
        var s = _a[_i];
        board[s.x][s.y] = s;
    }
}
// The main loop
function mainLoop() {
    draw();
    window.requestAnimationFrame(mainLoop);
}
mainLoop();
// All the queries that have to go to the server
function doMove(m) {
    $.ajax({
        url: "https://www.example.com",
        data: { move: [m.from.x, m.from.y, m.to.x, m.to.y] },
        success: updateState,
    });
    moveList = [new Move(m.to, m.from)];
    pieceList = [new Piece(m.to, 'bishop', 'white')];
    myTurn = true;
}
function getState() {
    $.ajax({
        url: "https://www.example.com",
        success: updateState,
    });
    moveList = [new Move(new Square(0, 0), new Square(2, 2))];
    pieceList = [new Piece(board[0][0], 'bishop', 'white')];
    myTurn = true;
}
//# sourceMappingURL=loka.js.map