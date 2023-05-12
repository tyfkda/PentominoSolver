use super::shape::Shape;

#[derive(Clone, Debug)]
pub struct Piece {
    pub name: char,
    pub shapes: Vec<Shape>,
}

impl Piece {
    pub fn create_pentominos() -> Vec<Self> {
        Shape::create_shapes().into_iter()
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
