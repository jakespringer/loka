// All the queries that have to go to the server

function doMove(m: Move) {
    $.ajax({
        url: "https://www.example.com",
        data: { move: [m.from.x, m.from.y, m.to.x, m.to.y] },
        success: updateState,
        //        error: function() { }
    });

    moveList = [new Move(m.to, m.from)];
    pieceList = [new Piece(m.to, 'bishop', 'white')]
    myTurn = true;
}

function getState() {
    $.ajax({
        url: "https://www.example.com",
        success: updateState,
        //        error: function() { }
    });

    moveList = [new Move(new Square(0, 0), new Square(2, 2))];
    pieceList = [new Piece(board[0][0], 'bishop', 'white')];
    myTurn = true;
}
