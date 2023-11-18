use super::shape::Shape;
use super::NUM_PIECES;

#[derive(Clone, Debug)]
pub struct Piece {
    pub name: char,
    pub shapes: Vec<Shape>,
}

impl Piece {
    pub fn create_pentominos(board_w: usize, board_h: usize) -> [Self; NUM_PIECES] {
        Shape::create_shapes(board_w, board_h)
            .map(|(name, shapes)| Self { name, shapes })
    }
}

#[derive(Clone, Debug, Default)]
pub struct PieceArrange {
    pub x: usize,
    pub y: usize,
    pub shape: usize,
}
