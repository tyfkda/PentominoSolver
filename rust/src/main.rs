mod pentomino;

use crate::pentomino::{Piece, placed_board, PieceArrange};
use crate::pentomino::solver::NaiveSolver;

fn main() {
    let pieces = Piece::create_pentominos();
    // for piece in &pieces {
    //     println!("{}: {:?}", piece.shapes.len(), piece);
    // }

    let w = 10;
    let h = 6;

    let f = move |pieces: &[Piece], arranges: &[&PieceArrange]| {
        let placed = placed_board(w, h, pieces, arranges);
        for y in 0..h {
            let s = placed[y * w .. y * w + w].iter().collect::<String>();
            println!("{:}", s);
        }
        println!("");
    };
    let mut solver = NaiveSolver::new(w, h, pieces, Box::new(f));
    solver.solve();
    println!("Total: Solution={}, check={}", solver.solution_count, solver.check_count);
}
