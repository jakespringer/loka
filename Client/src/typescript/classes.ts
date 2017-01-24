// All the classes used to store data

class Color {
    constructor(public r: number, public g: number, public b: number, public a = 1) { }
    toDrawingColor() {
        let f = (x: number) => Math.floor((x > 1 ? 1 : x) * 255);
        return { r: f(this.r), g: f(this.g), b: f(this.b), a: this.a };
    }
    toFillStyle(): string {
        let c = this.toDrawingColor();
        return 'rgba(' + c.r + ', ' + c.g + ', ' + c.b + ', ' + c.a + ')';
    }
    toString(): string {
        return 'Color{' + this.r + ',' + this.g + ',' + this.b + ',' + this.a + '}';
    }
}

class Piece {
    constructor(public square: Square, public pieceType: string, public team: string) { }
    toString(): string {
        return 'Piece{' + this.square + ',' + this.pieceType + '' + this.team + '}';
    }
}

class Square {
    constructor(public x: number, public y: number, public terrain?: string) { }
    add(other: Square): Square {
        return new Square(this.x + other.x, this.y + other.y);
    }
    equals(other: Square): boolean {
        return other != null && this.x == other.x && this.y == other.y;
    }
    getX(): number {
        return this.x * squareSize;
    }
    getY(): number {
        return this.y * squareSize;
    }
    multiply(c: number) {
        return new Square(this.x * c, this.y * c);
    }
    toString(): string {
        return 'Square{' + this.x + ',' + this.y + '}';
    }
}

class Move {
    constructor(public from: Square, public to: Square, public json?: MoveJSON) { }
    equals(other: Move): boolean {
        return this.from.equals(other.from) && this.to.equals(other.to);
    }
    toString(): string {
        return 'Move{' + this.from + ',' + this.to + '}'
    }
}

class AttackResult {
    constructor(public attackerDie: Die, public defenderDie: Die) { }
}

class Die {
    constructor(public sides: number, public roll: number) { }
}