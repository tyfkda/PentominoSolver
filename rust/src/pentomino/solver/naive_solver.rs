// Naive Solver: Depth First Search.

use std::collections::HashSet;

use crate::pentomino::{placed_board, BitBoard, Piece, PieceArrange, Shape, NUM_PIECES};

use super::{calc_hash, mirror_diag, mirror_x, mirror_y, Solver};

pub struct NaiveSolver {
    bitboard: BitBoard,
    w: usize,
    h: usize,
    pieces: [Piece; NUM_PIECES],
    found_callback: Box<dyn Fn(&[Piece], &[PieceArrange])>,

    arranges: [PieceArrange; NUM_PIECES],
    arranged: u32,
    pub check_count: usize,
    pub solution_count: usize,
    solution_hashes: HashSet<String>,
}

impl Solver for NaiveSolver {
    fn new(w: usize, h: usize, pieces: [Piece; NUM_PIECES], bitboard: BitBoard) -> Self {
        Self {
            bitboard,
            w,
            h,
            pieces,
            found_callback: Box::new(|_, _| {}),
            arranges: std::array::from_fn(|_| PieceArrange::default()),
            arranged: 0,
            check_count: 0,
            solution_count: 0,
            solution_hashes: HashSet::new(),
        }
    }

    fn set_callback(&mut self, callback: Box<dyn Fn(&[Piece], &[PieceArrange])>) {
        self.found_callback = callback;
    }

    fn solve(&mut self) -> (usize, usize) {
        self.check_count = 0;
        self.arranged = 0;

        // Check X piece first.
        if let Some(ip) = self.pieces.iter().position(|piece| piece.name == 'X') {
            self.solve_x(ip);
        } else {
            self.solve_recur(0, 0);
        }

        (self.solution_count, self.check_count)
    }
}

impl NaiveSolver {
    fn solve_x(&mut self, ip: usize) {
        // Put X piece first in top left quad.
        let is = 0;
        let (sw, sh, ofsy) = {
            let shape = &self.pieces[ip].shapes[0];
            (shape.w, shape.h, shape.ofsy)
        };
        let saved_bitboard = self.bitboard;
        for x in 0..=(self.w - sw) / 2 {
            for y in ofsy..=(self.h - sh) / 2 + ofsy {
                self.check_count += 1;
                let shape = &self.pieces[ip].shapes[is];
                if can_put_shape(saved_bitboard, self.w, self.h, shape, x, y) {
                    self.bitboard = put_shape(saved_bitboard, self.w, self.h, &self.pieces[ip].shapes[is], x, y - shape.ofsy);
                    self.arranges[ip] = PieceArrange {x, y: y - shape.ofsy, shape: 0};
                    self.arranged |= 1 << ip;
                    self.solve_recur(0, 0);
                }
            }
        }
    }

    fn solve_recur(&mut self, x: usize, y: usize) {
        self.check_count += 1;
        let saved_bitboard = self.bitboard;
        let mut order = self.arranged;
        loop {
            let ip = order.trailing_ones() as usize;
            if ip >= NUM_PIECES { break; }
            order |= 1 << ip;
            for is in 0..self.pieces[ip].shapes.len() {
                let shape = &self.pieces[ip].shapes[is];
                if can_put_shape(saved_bitboard, self.w, self.h, shape, x, y) {
                    self.bitboard = put_shape(saved_bitboard, self.w, self.h, shape, x, y - shape.ofsy);
                    self.arranges[ip] = PieceArrange {x, y: y - shape.ofsy, shape: is};
                    self.arranged |= 1 << ip;
                    if self.arranged != (1 << NUM_PIECES) - 1 {
                        let (nx, ny) = self.search_next_pos(x, y, self.bitboard);
                        self.solve_recur(nx, ny);
                    } else {
                        // All pieces are placed -> check whether this solution is unique.
                        let placed = placed_board(self.w, self.h, &self.pieces, &self.arranges);
                        if self.is_unique_solution(&placed) {
                            (self.found_callback)(&self.pieces, &self.arranges);
                            self.add_solution(&placed);
                        }
                    }
                }
            }
            self.arranged &= !(1 << ip);
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

    fn search_next_pos(&self, mut x: usize, mut y: usize, bitboard: BitBoard) -> (usize, usize) {
        loop {
            y += 1;
            if y >= self.h {
                y = 0;
                x += 1;
            }
            let index = y * self.w + x;
            if (bitboard & (1 << index)) == 0 { return (x, y); }
        }
    }
}

fn can_put_shape(board: BitBoard, w: usize, h: usize, shape: &Shape, x: usize, y: usize) -> bool {
    if x > w - shape.w { return false; }
    let ofsy = shape.ofsy;
    return y >= ofsy && y <= h + ofsy - shape.h &&
        (board & (shape.bitpat << ((y - ofsy) * w + x))) == 0;
}

fn put_shape(board: BitBoard, w: usize, _h: usize, shape: &Shape, x: usize, y: usize) -> BitBoard {
    let bitpat = shape.bitpat << (y * w + x);
    board | bitpat
}

