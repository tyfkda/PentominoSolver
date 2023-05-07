type BitPattern = u64;

#[derive(Clone, Debug, PartialEq)]
pub struct Shape {
    pub bitpat: BitPattern,
    pub w: usize,
    pub h: usize,
    pub ofsy: usize,
}

impl Shape {
    pub fn is_cell(&self, x: usize, y: usize) -> bool {
        (self.bitpat & (1 << (y * self.w + x))) != 0
    }

    pub fn line_bits(&self, y: usize) -> BitPattern {
        (self.bitpat >> (y * self.w)) & ((1 << self.w) - 1)
    }

    pub fn create_shapes() -> Vec<(char, Vec<Self>)> {
        // F, I, L, N, P, T, U, V, W, X, Y, Z
        let original_shapes = [
            OriginalShape {
                name: 'F',
                lines: Box::new([
                    " @@",
                    "@@ ",
                    " @ ",
                ]),
            },
            OriginalShape {
                name: 'I',
                lines: Box::new([
                    "@",
                    "@",
                    "@",
                    "@",
                    "@",
                ]),
            },
            OriginalShape {
                name: 'L',
                lines: Box::new([
                    "@ ",
                    "@ ",
                    "@ ",
                    "@@",
                ]),
            },
            OriginalShape {
                name: 'N',
                lines: Box::new([
                    " @",
                    " @",
                    "@@",
                    "@ ",
                ]),
            },
            OriginalShape {
                name: 'P',
                lines: Box::new([
                    "@@",
                    "@@",
                    "@ ",
                ]),
            },
            OriginalShape {
                name: 'T',
                lines: Box::new([
                    "@@@",
                    " @ ",
                    " @ ",
                ]),
            },
            OriginalShape {
                name: 'U',
                lines: Box::new([
                    "@ @",
                    "@@@",
                ]),
            },
            OriginalShape {
                name: 'V',
                lines: Box::new([
                    "@  ",
                    "@  ",
                    "@@@",
                ]),
            },
            OriginalShape {
                name: 'W',
                lines: Box::new([
                    "@  ",
                    "@@ ",
                    " @@",
                ]),
            },
            OriginalShape {
                name: 'X',
                lines: Box::new([
                    " @ ",
                    "@@@",
                    " @ ",
                ]),
            },
            OriginalShape {
                name: 'Y',
                lines: Box::new([
                    " @",
                    "@@",
                    " @",
                    " @",
                ]),
            },
            OriginalShape {
                name: 'Z',
                lines: Box::new([
                    "@@ ",
                    " @ ",
                    " @@",
                ]),
            },
        ];

        original_shapes
            .iter()
            .map(|pat| (pat.name, Self::create(pat)))
            .collect()
    }

    fn create(oshape: &OriginalShape) -> Vec<Self> {
        let base_shape = Self::from(&oshape.lines);

        // Create flipped and rotated shapes.
        let mut shapes = Vec::new();
        for i in 0..8 {
            let modified = Self::rot_flip(&base_shape, i >= 4, i & 3);
            if shapes.iter().all(|s| *s != modified) {
                shapes.push(modified)
            }
        }
        shapes
    }

    fn from(lines: &[&'static str]) -> Self {
        let w = lines[0].len();
        let h = lines.len();
        let mut bitpat = 0 as BitPattern;
        for y in 0..h {
            let bytes =  lines[y].as_bytes();
            for x in 0..w {
                let c =  bytes[w - x - 1];
                if c != 32 {
                    bitpat |= 1 << (y * w + x);
                }
            }
        }
        Shape { bitpat, w, h, ofsy: 0 }
    }

    fn rot_flip(base_shape: &Shape, flip: bool, rot: usize) -> Self {
        let mut shape = base_shape.clone();
        if flip {
            shape = Self::flip_y(&shape);
        }
        (0..rot).for_each(|_| {
            shape = Self::rot90(&shape);
        });
        shape.find_ofsy();
        shape
    }

    fn flip_y(shape: &Shape) -> Self {
        let mut bitpat = 0 as BitPattern;
        let line_mask = (1 << shape.w) - 1;
        for y in 0..shape.h {
            let line = (shape.bitpat >> ((shape.h - y - 1) * shape.w)) & line_mask;
            bitpat |= line << (y * shape.w);
        }
        Shape { bitpat, w: shape.w, h: shape.h, ofsy: 0 }
    }

    fn flip_diag(shape: &Shape) -> Self {
        let mut bitpat = 0 as BitPattern;
        for y in 0..shape.h {
            for x in 0..shape.w {
                let b = (shape.bitpat >> (y * shape.w + x)) & 1;
                bitpat |= b << (x * shape.h + y);
            }
        }
        Shape { bitpat, w: shape.h, h: shape.w, ofsy: 0 }
    }

    fn rot90(bshape: &Shape) -> Shape {
        Self::flip_diag(&Self::flip_y(bshape))
    }

    fn find_ofsy(&mut self) {
        self.ofsy = (0..self.h)
            .into_iter()
            .position(|y| self.is_cell(0, y))
            .unwrap()
    }
}

struct OriginalShape {
    name: char,
    lines: Box<[&'static str]>,
}
