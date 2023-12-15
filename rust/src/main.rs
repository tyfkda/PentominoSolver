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
    fn whb(&self) -> (usize, usize, BitBoard) {
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

    /// Print as figure
    #[arg(long)]
    figure: bool,

    /// Suppress solution output
    #[arg(short, long)]
    quiet: bool,
}

const GRID_CHARS: [char; 16] = [
    ' ', ' ', ' ', '━',
    ' ', '┏', '┓', '┳',
    ' ', '┗', '┛', '┻',
    '┃', '┣', '┫', '╋'];

fn print_result_figure(w: usize, h: usize, pieces: &[Piece], arranges: &[PieceArrange]) {
    let w2 = w + 1;
    let h2 = h * 2 + 1;

    let mut placed = {
        let place_piece = |mut placed: Vec<char>, (piece, arrange): (&Piece, &PieceArrange)| -> Vec<char> {
            // let base_index = arrange.y * w + arrange.x;
            let shape = &piece.shapes[arrange.shape];
            let c = piece.name;
            let mut bitpat = shape.bitpat;
            while bitpat != 0 {
                let ofs = bitpat.trailing_zeros() as usize;
                let xofs = ofs % w;
                let yofs = ofs / w;
                placed[((arrange.y + yofs) * 2 + 1) * w2 + (arrange.x + xofs + 1)] = c;
                bitpat = bitpat & (bitpat - 1);  // Remove lowest bit.
            }
            placed
        };

        let placed = vec!['.'; w2 * h2];
        pieces.iter().zip(arranges).fold(placed, place_piece)
    };

    let hlines: Vec<Vec<bool>> = (0..h).map(|y| {
        (0..(w + 1)).map(|x| {
            let c1 = if x > 0 { placed[(y * 2 + 1) * w2 +  x     ] } else { ' ' };
            let c2 = if x < w { placed[(y * 2 + 1) * w2 + (x + 1)] } else { ' ' };
            c1 != c2
        }).collect()
    }).collect();

    let vlines: Vec<Vec<bool>> = (0..(h + 1)).map(|y| {
        (0..w).map(|x| {
            let c1 = if y > 0 { placed[(y * 2 - 1) * w2 + (x + 1)] } else { ' ' };
            let c2 = if y < h { placed[(y * 2 + 1) * w2 + (x + 1)] } else { ' ' };
            c1 != c2
        }).collect()
    }).collect();

    // Put grid lines.
    for x in 0..(w + 1) {
        for y in 0..(h + 1) {
            let mut i = 0usize;
            if y < h && hlines[y    ][x] { i |= 1; }
            if y > 0 && hlines[y - 1][x] { i |= 2; }
            if x < w && vlines[y][x    ] { i |= 4; }
            if x > 0 && vlines[y][x - 1] { i |= 8; }
            placed[(y * 2) * w2 + x] = GRID_CHARS[i];
        }
    }
    for x in 0..(w + 1) {
        for y in 0..h {
            if hlines[y][x] {
                placed[(y * 2 + 1) * w2 + x] = GRID_CHARS[3];
            }
        }
    }

    // Transpose board to reduce lines.
    let mut letter_appeared = [false; 26];
    for x in 0..w2 {
        for y in 0..h2 {
            let i = y * w2 + x;
            let mut c = placed[i];
            if 'A' <= c && c <= 'Z' {
                let l = (c as usize) - ('A' as usize);
                if !letter_appeared[l] {
                    letter_appeared[l] = true;
                } else {
                    c = ' ';
                }
            }
            print!("{:}", c);
        }
        println!("");
    }
    println!("");
}

fn arrange_board(w: usize, h: usize, pieces: &[Piece], arranges: &[PieceArrange]) -> Vec<Option<(char, char)>> {
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

fn print_result_color(w: usize, h: usize, pieces: &[Piece], arranges: &[PieceArrange]) {
    let placed = arrange_board(w, h, &pieces, &arranges);
    // Transpose board to reduce lines.
    for x in 0..w {
        for y in 0..h {
            if let Some((name, c)) = placed[y * w + x] {
                let cs = {
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
                };
                print!("{cs}");
            } else {
                print!(". ");
            }
        }
        println!("");
    }
    println!("");
}

fn solve<T: Solver>(args: &Args) {
    let size = args.size.unwrap_or(BoardSize::_6x10);
    let (w, h, initial_bitboard) = size.whb();
    let pieces = Piece::create_pentominos(w, h);
    let mut solver: T = T::new(w, h, pieces, initial_bitboard);

    if !args.quiet {
        let disp_color = !args.figure && atty::is(Stream::Stdout);
        let f = move |pieces: &[Piece], arranges: &[PieceArrange]| {
            if disp_color {
                print_result_color(w, h, pieces, arranges);
            } else {
                print_result_figure(w, h, pieces, arranges);
            }
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
