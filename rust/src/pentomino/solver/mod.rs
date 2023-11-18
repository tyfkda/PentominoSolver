mod dlx_solver;
mod naive_solver;

pub use dlx_solver::DlxSolver;
pub use naive_solver::NaiveSolver;

use super::{Piece, PieceArrange};

pub trait Solver {
    fn set_callback(&mut self, callback: Box<dyn Fn(&[Piece], &[PieceArrange])>);
    fn solve(&mut self) -> (usize, usize);
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
