mod pentomino;

use atty::Stream;
use clap::Parser;
use colored::Colorize;

use crate::pentomino::solver::{DlxSolver, NaiveSolver, Solver};
use crate::pentomino::{BitBoard, Piece, PieceArrange};

use std::time::Instant;

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
            "6x10" | "6" | "10x6" => Ok(BoardSize::_6x10),
            "5x12" | "5" | "12x5" => Ok(BoardSize::_5x12),
            "4x15" | "4" | "15x4" => Ok(BoardSize::_4x15),
            "3x20" | "3" | "20x3" => Ok(BoardSize::_3x20),
            "8x8"  | "8"          => Ok(BoardSize::_8x8),
            _ => Err(String::from("Illegal size")),
        }
    }
}

#[derive(Parser, Debug)]
#[command(about, long_about = None)]
struct Args {
    /// Board size (6x10, 5x12, 4x15, 3x20, 8x8 (default: 6x10))
    #[arg(short, long, value_parser = BoardSize::parse)]
    size: Option<BoardSize>,

    /// Use Dancing Links algorithm
    #[arg(long)]
    dlx: bool,

    /// Suppress solution output
    #[arg(short, long)]
    quiet: bool,
}

fn color_board(w: usize, h: usize, pieces: &[Piece], arranges: &[PieceArrange]) -> Vec<Option<(char, char)>> {
    let place_piece = |mut placed: Vec<Option<(char, char)>>, (piece, arrange): (&Piece, &PieceArrange)| -> Vec<Option<(char, char)>> {
        let base_index = arrange.y * w + arrange.x;
        let shape = &piece.shapes[arrange.shape];
        let mut c = piece.name;
        let mut bitpat = shape.bitpat;
        while bitpat != 0 {
            let ofs = bitpat.trailing_zeros() as usize;
            placed[base_index + ofs] = Some((piece.name, c));
            c = ' ';
            bitpat = bitpat & (bitpat - 1);  // Remove lowest bit.
        }
        placed
    };

    let placed = vec![None; w * h];
    pieces.iter().zip(arranges).fold(placed, place_piece)
}

fn print_result(w: usize, h: usize, tty: bool, pieces: &[Piece], arranges: &[PieceArrange]) {
    let placed = color_board(w, h, &pieces, &arranges);
    for y in 0..h {
        for x in 0..w {
            if let Some((name, c)) = placed[y * w + x] {
                let cs = if tty {
                    let s = String::from(c) + " ";
                    match name {
                        'F' => s.on_bright_red(),
                        'I' => s.on_purple().bright_white(),
                        'L' => s.on_bright_yellow(),
                        'N' => s.on_bright_blue(),
                        'P' => s.on_bright_purple(),
                        'T' => s.on_bright_cyan(),
                        'U' => s.on_red().bright_white(),
                        'V' => s.on_blue().bright_white(),
                        'W' => s.on_yellow().bright_white(),
                        'X' => s.on_bright_green(),
                        'Y' => s.on_green().bright_white(),
                        _ => s.on_cyan().bright_white(),
                    }
                } else {
                    String::from(name).normal()
                };
                print!("{cs}");
            } else {
                let c = if tty { ". " } else { "." };
                print!("{c}");
            }
        }
        println!("");
    }
    println!("");
}

fn solve<T: Solver>(args: &Args) {
    let size = args.size.unwrap_or(BoardSize::_6x10);
    let (h, w, initial_bitboard) = size.hwb();
    let pieces = Piece::create_pentominos(w, h);
    let mut solver: T = T::new(w, h, pieces, initial_bitboard);

    if !args.quiet {
        let tty = atty::is(Stream::Stdout);
        let f = move |pieces: &[Piece], arranges: &[PieceArrange]| {
            print_result(w, h, tty, pieces, arranges);
        };
        solver.set_callback(Box::new(f));
    }

    let now = Instant::now();
    let (solution_count, check_count) = solver.solve();
    let elapsed = now.elapsed();

    println!("Total: Solution={}, check={}, elapsed={elapsed:?}", solution_count, check_count);
}

fn main() {
    let args = Args::parse();

    if args.dlx {
        solve::<DlxSolver>(&args);
    } else {
        solve::<NaiveSolver>(&args);
    }
}
