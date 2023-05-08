mod pentomino;

use clap::Parser;

use crate::pentomino::{BitBoard, Piece, PieceArrange, placed_board};
use crate::pentomino::solver::{DlxSolver, NaiveSolver, Solver};

#[derive(Clone, Copy, Debug, clap::ValueEnum)]
enum BoardSize { _6x10, _5x12, _4x15, _3x20, _8x8 }

impl BoardSize {
    fn hwb(&self) -> (usize, usize, BitBoard) {
        match self {
            BoardSize::_6x10 => (6, 10, 0),
            BoardSize::_5x12 => (5, 12, 0),
            BoardSize::_4x15 => (4, 15, 0),
            BoardSize::_3x20 => (3, 20, 0),
            BoardSize::_8x8  => (8, 8, (1 << (3 * 8 + 3)) | (1 << (3 * 8 + 4)) | (1 << (4 * 8 + 3)) | (1 << (4 * 8 + 4))),
        }
    }

    fn parse(s: &str) -> Result<BoardSize, String> {
        match s {
            "6x10" | "6" => Ok(BoardSize::_6x10),
            "5x12" | "5" => Ok(BoardSize::_5x12),
            "4x15" | "4" => Ok(BoardSize::_4x15),
            "3x20" | "3" => Ok(BoardSize::_3x20),
            "8x8"  | "8" => Ok(BoardSize::_8x8),
            _ => Err(String::from("Illegal size")),
        }
    }
}

#[derive(Parser, Debug)]
#[command(about, long_about = None)]
struct Args {
    /// Board size (10x6, 12x5, 15x4, 20x3, 8x8 (default: 10x6))
    #[arg(short, long, value_parser = BoardSize::parse)]
    size: Option<BoardSize>,

    #[arg(long)]
    dlx: bool,
}

fn solve(solver: &mut impl Solver, w: usize, h: usize) {
    let f = move |pieces: &[Piece], arranges: &[&PieceArrange]| {
        let placed = placed_board(w, h, &pieces, &arranges);
        for y in 0..h {
            let s = placed[y * w .. y * w + w].iter().collect::<String>();
            println!("{:}", s);
        }
        println!("");
    };
    solver.set_callback(Box::new(f));

    let (solution_count, check_count) = solver.solve();
    println!("Total: Solution={}, check={}", solution_count, check_count);
}

fn main() {
    let args = Args::parse();
    let size = args.size.unwrap_or(BoardSize::_6x10);
    let (h, w, initial_bitboard) = size.hwb();

    let pieces = Piece::create_pentominos();
    // for piece in &pieces {
    //     println!("{}: {:?}", piece.shapes.len(), piece);
    // }

    if args.dlx {
        let mut solver: DlxSolver = DlxSolver::new(w, h, pieces, initial_bitboard);
        solve(&mut solver, w, h);
    } else {
        let mut solver = NaiveSolver::new(w, h, pieces, initial_bitboard);
        solve(&mut solver, w, h);
    }
}
