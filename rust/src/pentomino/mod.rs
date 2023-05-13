pub mod piece;
pub mod shape;
pub mod solver;

pub use piece::{Piece, PieceArrange};
pub use shape::Shape;

pub type BitBoard = u64;

pub fn placed_board(w: usize, h: usize, pieces: &[Piece], arranges: &[&PieceArrange]) -> Vec<char> {
    let place_piece = |mut placed: Vec<char>, (piece, arrange): (&Piece, &&PieceArrange)| -> Vec<char> {
        let x = arrange.x;
        let y = arrange.y;
        let shape = &piece.shapes[arrange.shape];
        for i in 0..shape.h {
            for j in 0..shape.w {
                if shape.is_cell(j, i, w) {
                    placed[(y + i) * w + x + j] = piece.name;
                }
            }
        }
        placed
    };

    let placed = vec![' '; w * h];
    pieces.iter().zip(arranges).fold(placed, place_piece)
}
