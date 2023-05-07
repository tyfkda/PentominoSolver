use std::collections::HashSet;

use crate::pentomino::{BitBoard, Piece, PieceArrange, Shape, placed_board};

pub struct NaiveSolver {
    bitboard: BitBoard,
    w: usize,
    h: usize,
    pieces: Vec<Piece>,
    found_callback: Box<dyn Fn(&[Piece], &[&PieceArrange])>,

    arranges: Vec<Option<PieceArrange>>,
    pub check_count: usize,
    pub solution_count: usize,
    solution_hashes: HashSet<String>,
}

impl NaiveSolver {
    pub fn new(w: usize, h: usize, pieces: Vec<Piece>, bitboard: BitBoard, found_callback: Box<dyn Fn(&[Piece], &[&PieceArrange])>) -> Self {
        let n = pieces.len();
        Self {
            bitboard,
            w,
            h,
            pieces,
            found_callback,
            arranges: vec![None; n],
            check_count: 0,
            solution_count: 0,
            solution_hashes: HashSet::new(),
        }
    }

    pub fn solve(&mut self) {
        self.check_count = 0;
        self.arranges.fill(None);
        self.solve_recur(0, 0);
    }

    fn solve_recur(&mut self, x: usize, y: usize) {
        self.check_count += 1;
        for ip in 0..self.pieces.len() {
          if self.arranges[ip].is_some() {
            continue;
          }
          for is in 0..self.pieces[ip].shapes.len() {
            if let Some(ofsy) = can_put_shape(self.bitboard, self.w, self.h, &self.pieces[ip].shapes[is], x, y) {
                self.bitboard = put_shape(self.bitboard, self.w, self.h, &self.pieces[ip].shapes[is], x, y - ofsy, true);
                self.arranges[ip] = Some(PieceArrange {x, y: y - ofsy, shape: is});
                if let Some((nx, ny)) = self.search_next_pos(x, y) {
                    self.solve_recur(nx, ny);
                } else {
                    let (placed, uniq) = {
                        let arranges = self.arranges.iter().map(|e| e.as_ref().unwrap()).collect::<Vec<_>>();
                        let placed = placed_board(self.w, self.h, &self.pieces, &arranges);
                        let mut uniq = false;
                        if self.is_unique_solution(&placed) {
                            uniq = true;
                            (self.found_callback)(&self.pieces, &arranges);
                        }
                        (placed, uniq)
                    };
                    if uniq {
                        self.add_solution(&placed);
                    }
                }
                self.bitboard = put_shape(self.bitboard, self.w, self.h, &self.pieces[ip].shapes[is], x, y - ofsy, false);
            }
          }
          self.arranges[ip] = None;
        }
    }

    fn is_unique_solution(&self, placed: &Vec<char>) -> bool {
        let hash = calc_hash(placed);
        !self.solution_hashes.contains(&hash)
    }

    fn add_solution(&mut self, placed: &Vec<char>) {
        self.solution_count += 1;

        // Register mirrored.
        let mut mirrored = placed.clone();
        mirror_y(&mut mirrored, self.w, self.h);
        self.solution_hashes.insert(calc_hash(&mirrored));
        mirror_x(&mut mirrored, self.w, self.h);
        self.solution_hashes.insert(calc_hash(&mirrored));
        mirror_y(&mut mirrored, self.w, self.h);
        self.solution_hashes.insert(calc_hash(&mirrored));

        if self.w == self.h {
            // Register rotated.
            mirror_diag(&mut mirrored, self.w);
            self.solution_hashes.insert(calc_hash(&mirrored));
            mirror_y(&mut mirrored, self.w, self.h);
            self.solution_hashes.insert(calc_hash(&mirrored));
            mirror_x(&mut mirrored, self.w, self.h);
            self.solution_hashes.insert(calc_hash(&mirrored));
            mirror_y(&mut mirrored, self.w, self.h);
            self.solution_hashes.insert(calc_hash(&mirrored));
        }
    }

    fn search_next_pos(&self, mut x: usize, mut y: usize) -> Option<(usize, usize)> {
        loop {
            y += 1;
            if y >= self.h {
                y = 0;
                x += 1;
                if x >= self.w { return None }
            }
            let index = y * self.w + x;
            if (self.bitboard & (1 << index)) == 0 { return Some((x, y)) }
        }
    }
}

fn can_put_shape(board: BitBoard, w: usize, h: usize, shape: &Shape, x: usize, y: usize) -> Option<usize> {
    if x > w - shape.w { return None; }
    let ofsy = shape.ofsy;
    if y < ofsy || y > h + ofsy - shape.h { return None; }
    for i in 0..shape.h {
        let line = shape.line_bits(i);
        if (board & (line << ((y + i - ofsy) * w + x))) != 0 { return None }
    }
    Some(ofsy)
}

fn put_shape(mut board: BitBoard, w: usize, _h: usize, shape: &Shape, x: usize, y: usize, set: bool) -> BitBoard {
    for i in 0..shape.h {
        let line = shape.line_bits(i) << ((y + i) * w + x);
        if set {
            board |= line;
        } else {
            board &= !line;
        }
    }
    board
}

fn calc_hash(placed: &Vec<char>) -> String {
    // TODO: Use hash function.
    placed.iter().collect::<String>()
}

fn mirror_x(placed: &mut Vec<char>, w: usize, h: usize) {
    let m = w >> 1;
    for y in 0..h {
        for x in 0..m {
            let x2 = w - x - 1;
            let t = placed[y * w + x];
            placed[y * w + x] = placed[y * w + x2];
            placed[y * w + x2] = t;
        }
    }
}

fn mirror_y(placed: &mut Vec<char>, w: usize, h: usize) {
    let m = h >> 1;
    for x in 0..w {
        for y in 0..m {
            let y2 = h - y - 1;
            let t = placed[y * w + x];
            placed[y * w + x] = placed[y2 * w + x];
            placed[y2 * w + x] = t;
        }
    }
}

fn mirror_diag(placed: &mut Vec<char>, w: usize) {
    assert!(placed.len() == w * w);
    for y in 0..(w - 1) {
        for x in (y + 1)..w {
            let t = placed[y * w + x];
            placed[y * w + x] = placed[x * w + y];
            placed[x * w + y] = t;
        }
    }
}
