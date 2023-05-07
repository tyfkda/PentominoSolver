use crate::pentomino::{BitBoard, Piece, PieceArrange, Shape};

pub struct NaiveSolver {
    bitboard: BitBoard,
    w: usize,
    h: usize,
    pieces: Vec<Piece>,
    found_callback: Box<dyn Fn(&[Piece], &[&PieceArrange])>,

    arranges: Vec<Option<PieceArrange>>,
    pub check_count: usize,
    pub solution_count: usize,
}

impl NaiveSolver {
    pub fn new(w: usize, h: usize, pieces: Vec<Piece>, found_callback: Box<dyn Fn(&[Piece], &[&PieceArrange])>) -> Self {
        let n = pieces.len();
        Self {
            bitboard: 0,
            w,
            h,
            pieces,
            found_callback,
            arranges: vec![None; n],
            check_count: 0,
            solution_count: 0,
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
                } else if self.is_unique_solution() {
                    self.add_solution();
                    let arranges = self.arranges.iter().map(|e| e.as_ref().unwrap()).collect::<Vec<_>>();
                    (self.found_callback)(&self.pieces, &arranges);
                }
                self.bitboard = put_shape(self.bitboard, self.w, self.h, &self.pieces[ip].shapes[is], x, y - ofsy, false);
            }
          }
          self.arranges[ip] = None;
        }
    }

    fn is_unique_solution(&self) -> bool {
        true
    }

    fn add_solution(&mut self) {
        self.solution_count += 1;
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
