pub mod piece;
pub mod shape;
pub mod solver;

pub use piece::{Piece, PieceArrange};
pub use shape::Shape;

pub type BitBoard = u64;

pub fn placed_board(w: usize, h: usize, pieces: &[Piece], arranges: &[&PieceArrange]) -> Vec<char> {
    let place_piece = |mut placed: Vec<char>, (piece, arrange): (&Piece, &&PieceArrange)| -> Vec<char> {
        let base_index = arrange.y * w + arrange.x;
        let shape = &piece.shapes[arrange.shape];
        let mut bitpat = shape.bitpat;
        while bitpat != 0 {
            let ofs = bitpat.trailing_zeros() as usize;
            placed[base_index + ofs] = piece.name;
            bitpat = bitpat & (bitpat - 1);  // Remove lowest bit.
        }
        placed
    };

    let placed = vec![' '; w * h];
    pieces.iter().zip(arranges).fold(placed, place_piece)
}

pub fn delta_swap(x: BitBoard, mask: u64, delta: usize) -> BitBoard {
    let t = ((x >> delta) ^ x) & mask;
    t ^ (t << delta) ^ x
}
