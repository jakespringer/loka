///<reference path='images.ts' />

let canvas = document.createElement("canvas");
document.body.appendChild(canvas);
let ctx = canvas.getContext("2d");

// The game state

let moveList: Move[] = [];

let pieceList: Piece[] = [];

let board: Square[][] = [];
let boardSize = 12;
for (let x = 0; x < boardSize; x++) {
    board.push([]);
    for (let y = 0; y < boardSize; y++) {
        board[x].push(new Square(x, y));
    }
}

let squareSize: number = getSquareSize();

let selected: Piece = null;
let selectedOffset = { x: 0, y: 0 };

let mousePos = { x: 0, y: 0 };

let myTurn = false;

getState();

// Responding to user input

canvas.addEventListener("mousedown", function(e) {
    if (myTurn && selected == null) {
        let s = getSquare(e.clientX, e.clientY);
        if (getPieceOnSquare(s) != null) {
            selected = getPieceOnSquare(s);
            selectedOffset = { x: e.clientX - s.getX(), y: e.clientY - s.getY() }
        }
    }
});

canvas.addEventListener("mousemove", function(e) {
    mousePos = { x: e.clientX, y: e.clientY };
});

canvas.addEventListener("mouseup", function(e) {
    if (selected != null) {
        let s = getSquare(e.clientX, e.clientY);
        for (let m of moveList) {
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
    let width = ctx.canvas.width = window.innerWidth;
    let height = ctx.canvas.height = window.innerHeight;

    ctx.clearRect(0, 0, width, height);

    squareSize = getSquareSize();

    for (let x = 0; x < boardSize; x++) {
        for (let y = 0; y < boardSize; y++) {
            let c = (x % 2 + y % 2 == 1) ? new Color(.9, .9, .9) : new Color(.2, .2, .2);
            ctx.fillStyle = c.toFillStyle();
            ctx.fillRect(x * squareSize, y * squareSize, squareSize, squareSize);
            ctx.fillStyle = "red";
            ctx.fillText(x + ' ' + y, x * squareSize, (y + 1) * squareSize);
        }
    }

    for (let p of pieceList) {
        if (p != selected) {
            ctx.drawImage(sprites[p.team][p.pieceType], p.square.getX(), p.square.getY(), squareSize, squareSize);
        }
    }
    if (selected != null) {
        for (let m of moveList) {
            if (m.from.equals(selected.square)) {
                ctx.fillStyle = new Color(1, 1, 0, .5).toFillStyle();
                ctx.fillRect(m.to.x * squareSize, m.to.y * squareSize, squareSize, squareSize);
            }
        }
        ctx.drawImage(sprites[selected.team][selected.pieceType], mousePos.x - selectedOffset.x, mousePos.y - selectedOffset.y, squareSize, squareSize);
    }
}

// Misc utility functions

function getSquare(x: number, y: number): Square {
    if (Math.min(x, y) < 0 || Math.max(x, y) >= squareSize * boardSize) {
        return null;
    }
    return board[Math.floor(x / squareSize)][Math.floor(y / squareSize)];
}

function getSquareSize(): number {
    return Math.min(window.innerWidth, window.innerHeight) / 12;
}

function getPieceOnSquare(s: Square): Piece {
    for (let p of pieceList) {
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
