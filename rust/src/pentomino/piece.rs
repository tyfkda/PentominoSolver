use super::shape::Shape;

#[derive(Clone, Debug)]
pub struct Piece {
    pub name: char,
    pub shapes: Vec<Shape>,
}

impl Piece {
    pub fn create_pentominos(board_w: usize, board_h: usize) -> Vec<Self> {
        Shape::create_shapes(board_w, board_h).into_iter()
            .map(|(name, shapes)| {
                Self {
                    name,
                    shapes,
                }
            })
            .collect()
    }
}

#[derive(Clone, Debug, Default)]
pub struct PieceArrange {
    pub x: usize,
    pub y: usize,
    pub shape: usize,
}
