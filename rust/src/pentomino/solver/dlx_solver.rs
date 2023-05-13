// Dancing Links Solver

use std::collections::HashSet;

// use core::iter;
// use std::collections::HashSet;
use dancing_links::{ExactCover};

use crate::pentomino::{Piece, BitBoard, PieceArrange, placed_board};

use super::{Solver, calc_hash, mirror_diag, mirror_x, mirror_y};

pub struct DlxSolver {
    /// The list of possible values and positions that are valid for this Sudoku
    /// puzzle.
    pub possibilities: Vec<Possibility>,
    /// The list of constraints that must be satisfied for this Sudoku puzzle.
    pub constraints: Vec<Constraint>,

    w: usize,
    h: usize,
    pieces: Vec<Piece>,
    found_callback: Box<dyn Fn(&[Piece], &[&PieceArrange])>,
}

impl Solver for DlxSolver {
    fn set_callback(&mut self, callback: Box<dyn Fn(&[Piece], &[&PieceArrange])>) {
        self.found_callback = callback;
    }

    fn solve(&mut self) -> (usize, usize) {
        let solver = self.solver();
        let mut total = 0;
        let mut solution_hashes = HashSet::<String>::new();
        for solution in solver {
            let mut arranges: Vec<PieceArrange> = vec![PieceArrange::default(); solution.len()];
            for p in solution {
                arranges[p.piece] = PieceArrange {x: p.x, y: p.y, shape: p.shape};
            }

            let placed = placed_board(self.w, self.h, &self.pieces, &arranges.iter().map(|a| a).collect::<Vec<_>>());
            let hash = calc_hash(&placed);
            if solution_hashes.contains(&hash) {
                continue;
            }
            self.add_solution(&mut solution_hashes, &placed);

            let solp: Vec<&PieceArrange> = arranges.iter().map(|a| a).collect();
            (self.found_callback)(&self.pieces, &solp);
            total += 1;
        }

        (total, 0)
    }
}

impl DlxSolver {
    pub fn new(w: usize, h: usize, pieces: Vec<Piece>, bitboard: BitBoard) -> Self {
        let possibilities = Possibility::all(w, h, &pieces, bitboard).collect();
        let constraints = Constraint::all(w, h, bitboard, pieces.len()).collect();

        Self {
            pieces,
            possibilities,
            constraints,

            w,
            h,
            found_callback: Box::new(|_, _| {}),
        }
    }

    fn add_solution(&self, solution_hashes: &mut HashSet<String>, placed: &Vec<char>) {
        // Register mirrored.
        let mut mirrored = placed.clone();
        mirror_y(&mut mirrored, self.w, self.h);
        solution_hashes.insert(calc_hash(&mirrored));
        mirror_x(&mut mirrored, self.w, self.h);
        solution_hashes.insert(calc_hash(&mirrored));
        mirror_y(&mut mirrored, self.w, self.h);
        solution_hashes.insert(calc_hash(&mirrored));

        if self.w == self.h {
            // Register rotated.
            mirror_diag(&mut mirrored, self.w);
            solution_hashes.insert(calc_hash(&mirrored));
            mirror_y(&mut mirrored, self.w, self.h);
            solution_hashes.insert(calc_hash(&mirrored));
            mirror_x(&mut mirrored, self.w, self.h);
            solution_hashes.insert(calc_hash(&mirrored));
            mirror_y(&mut mirrored, self.w, self.h);
            solution_hashes.insert(calc_hash(&mirrored));
        }
    }
}

impl ExactCover for DlxSolver {
    type Possibility = Possibility;
    type Constraint = Constraint;

    fn satisfies(&self, poss: &Self::Possibility, cons: &Self::Constraint) -> bool {
        poss.satisfies(cons, &self.pieces, self.w)
    }

    fn is_optional(&self, _cons: &Self::Constraint) -> bool {
        false
    }

    fn possibilities(&self) -> &[Self::Possibility] {
        &self.possibilities
    }

    fn constraints(&self) -> &[Self::Constraint] {
        &self.constraints
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Possibility {
    pub piece: usize,
    pub shape: usize,
    pub x: usize,
    pub y: usize,
}

impl Possibility {
    pub fn all(w: usize, h: usize, pieces: &Vec<Piece>, bitboard: BitBoard) -> impl Iterator<Item = Self> + '_ {
        pieces.iter().enumerate()
            .flat_map(move |(ip, piece)| piece.shapes.iter().enumerate()
            .flat_map(move |(is, shape)| {
                let ww = if shape.w <= w { w - shape.w + 1 } else { 0 };
                let hh = if shape.h <= h { h - shape.h + 1 } else { 0 };
                (0..ww * hh)
                    .filter_map(move |i| {
                        let x = i % ww;
                        let y = i / ww;
                        if (bitboard & (shape.bitpat << (y * w + x))) != 0 {
                            return None;
                        }
                        Some(Possibility {piece: ip, shape: is, x, y})
                    })
            }))
    }

    /// Return true if this `Possibility` satisfies the given `Constraint`.
    pub fn satisfies(&self, constraint: &Constraint, pieces: &Vec<Piece>, board_w: usize) -> bool {
        match constraint {
            &Constraint::Cell(x, y) => {
                if x < self.x || y < self.y { return false; }
                let shape = &pieces[self.piece].shapes[self.shape];
                let xx = x - self.x;
                let yy = y - self.y;
                xx < shape.w && yy < shape.h && shape.is_cell(xx, yy, board_w)
            },
            &Constraint::Piece(piece) => self.piece == piece,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Constraint {
    Cell(usize, usize),
    Piece(usize),
}

impl Constraint {
    fn all(w: usize, h: usize, bitboard: BitBoard, piece_count: usize) -> impl Iterator<Item = Constraint> {
        let vcell = (0..w * h)
            .filter_map(move |i| {
                let x = i % w;
                let y = i / w;
                if bitboard & (1 << (y * w + x)) != 0 { None } else { Some(Constraint::Cell(x, y)) }
            });
        let vpiece = (0..piece_count)
            .map(move |i| Constraint::Piece(i));
        vcell.chain(vpiece)
    }
}
