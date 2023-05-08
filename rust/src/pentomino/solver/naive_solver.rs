use std::collections::HashSet;

use crate::pentomino::{BitBoard, Piece, PieceArrange, Shape, placed_board};

use super::{Solver, calc_hash, mirror_diag, mirror_x, mirror_y};

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

impl Solver for NaiveSolver {
    fn set_callback(&mut self, callback: Box<dyn Fn(&[Piece], &[&PieceArrange])>) {
        self.found_callback = callback;
    }

    fn solve(&mut self) -> (usize, usize) {
        self.check_count = 0;
        self.arranges.fill(None);

        // Check X piece first.
        if let Some(ip) = self.pieces.iter().position(|piece| piece.shapes.len() == 1) {
            self.solve_x(ip);
        } else {
            self.solve_recur(0, 0);
        }

        (self.solution_count, self.check_count)
    }
}

impl NaiveSolver {
    pub fn new(w: usize, h: usize, pieces: Vec<Piece>, bitboard: BitBoard) -> Self {
        let n = pieces.len();
        Self {
            bitboard,
            w,
            h,
            pieces,
            found_callback: Box::new(|_, _| {}),
            arranges: vec![None; n],
            check_count: 0,
            solution_count: 0,
            solution_hashes: HashSet::new(),
        }
    }

    fn solve_x(&mut self, ip: usize) {
        // Put X piece first in top left quad.
        let is = 0;
        for x in 0..(self.w + 1) / 2 {
            for y in 0..(self.h + 1) / 2 {
                if x == 0 && y == 0 { continue; }
                self.check_count += 1;
                if let Some(ofsy) = can_put_shape(self.bitboard, self.w, self.h, &self.pieces[ip].shapes[is], x, y) {
                    self.bitboard = put_shape(self.bitboard, self.w, self.h, &self.pieces[ip].shapes[is], x, y - ofsy, true);
                    self.arranges[ip] = Some(PieceArrange {x, y: y - ofsy, shape: 0});
                    self.solve_recur(0, 0);
                    self.bitboard = put_shape(self.bitboard, self.w, self.h, &self.pieces[ip].shapes[is], x, y - ofsy, false);
                }
            }
        }
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
