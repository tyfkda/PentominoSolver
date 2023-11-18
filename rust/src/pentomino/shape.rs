use super::{delta_swap, BitBoard, NUM_PIECES};

// Bit pattern is stored as multiple rows,
// and its line width is same as board_w.

#[derive(Clone, Debug, PartialEq)]
pub struct Shape {
    pub bitpat: BitBoard,
    pub w: usize,
    pub h: usize,
    pub ofsy: usize,
}

impl Shape {
    pub fn is_cell(&self, x: usize, y: usize, board_w: usize) -> bool {
        (self.bitpat & (1 << (y * board_w + x))) != 0
    }

    pub fn create_shapes(board_w: usize, board_h: usize) -> [(char, Vec<Self>); NUM_PIECES] {
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
            .map(|pat| (pat.name, Self::create(&pat, board_w, board_h)))
    }

    fn create(oshape: &OriginalShape, board_w: usize, board_h: usize) -> Vec<Self> {
        // Line width of bit pattern is pattern's width,
        // and convert to board_w using `to_board_width`.

        let base_shape = Self::from(&oshape.lines);

        // Create flipped and rotated shapes.
        let mut shapes = Vec::new();
        for i in 0..8 {
            let rotflipped = Self::rot_flip(&base_shape, i >= 4, i & 3);
            if rotflipped.w > board_w || rotflipped.h > board_h { continue; }
            let modified = Self {
                bitpat: Self::to_board_width(&rotflipped, board_w),
                w: rotflipped.w,
                h: rotflipped.h,
                ofsy: Self::find_ofsy(rotflipped.bitpat, rotflipped.w, rotflipped.h),
            };
            if shapes.iter().all(|s| *s != modified) {
                shapes.push(modified)
            }
        }
        shapes
    }

    fn to_board_width(shape: &Shape, board_w: usize) -> BitBoard {
        assert!(shape.w <= board_w);
        let mut bitpat = shape.bitpat;
        if shape.w < board_w {
            for y in (1..shape.h).rev() {
                let mask = (1 << (y * shape.w)) - 1;
                bitpat = (bitpat & mask) | ((bitpat & !mask) << (board_w - shape.w));
            }
        }
        bitpat
    }

    fn from(lines: &[&'static str]) -> Self {
        let w = lines[0].len();
        let h = lines.len();
        let mut bitpat = 0 as BitBoard;
        for y in 0..h {
            let bytes = lines[y].as_bytes();
            for x in 0..w {
                let c = bytes[w - x - 1];
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
        shape
    }

    fn flip_y(shape: &Shape) -> Self {
        let mut bitpat = shape.bitpat;
        let line_mask = (1 << shape.w) - 1;
        for y in 0..shape.h / 2 {
            bitpat = delta_swap(bitpat, line_mask << (y * shape.w), (shape.h - 1 - 2 * y) * shape.w);
        }
        Shape { bitpat, w: shape.w, h: shape.h, ofsy: 0 }
    }

    fn rot90(shape: &Shape) -> Self {
        let mut bitpat = 0 as BitBoard;
        for y in 0..shape.w {
            for x in 0..shape.h {
                let b = (shape.bitpat >> ((shape.h - x - 1) * shape.w + y)) & 1;
                bitpat |= b << (y * shape.h + x);
            }
        }
        Shape { bitpat, w: shape.h, h: shape.w, ofsy: 0 }
    }

    fn find_ofsy(bitpat: BitBoard, w: usize, h: usize) -> usize {
        (0..h)
            .into_iter()
            .position(|y| (bitpat & (1 << (y * w))) != 0)
            .unwrap()
    }
}

struct OriginalShape {
    name: char,
    lines: Box<[&'static str]>,
}
