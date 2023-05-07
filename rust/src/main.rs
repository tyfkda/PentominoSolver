mod pentomino;

use pentomino::Piece;

fn main() {
    let pieces = Piece::create_pentominos();
    for piece in &pieces {
        println!("{}: {:?}", piece.shapes.len(), piece);
    }
}
