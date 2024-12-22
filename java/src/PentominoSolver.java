import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import pentomino.BoardConfig;
import pentomino.PieceArrange;
import pentomino.Solver;
import pentomino.BitBoard;
import pentomino.component.Piece;
import pentomino.component.Shape;

public class PentominoSolver {
    public static void main(String[] args_) throws Exception {
        Args args = parseArgs(args_);
        if (args == null)
            System.exit(1);

        int boardW = args.boardConfig.width;
        int boardH = args.boardConfig.height;
        Piece[] pieces = Piece.createShapes(boardW, boardH);

        Solver solver = new Solver(args.boardConfig, pieces);
        if (!args.quiet) {
            Solver.Callback callback;
            if (args.figure)
                callback = new PrintResultFigure(pieces, args.boardConfig);
            else
                callback = new PrintResultColor(pieces, args.boardConfig);
            solver.setCallback(callback);
        }

        long startTime = System.nanoTime();
        solver.solve();
        long elapsed = System.nanoTime() - startTime;

        System.out.printf("Total: Solution=%d, check=%d, elapsed=%fms\n", solver.solutionCount, solver.checkCount, elapsed / 1000000.0);
    }

    private static Args parseArgs(String[] args) {
        BoardConfig boardConfig = BoardConfig._6x10;
        boolean figure = false;
        boolean quiet = false;
        int i;
        for (i = 0; i < args.length; ++i) {
            if ((args[i].equals("-s") || args[i].equals("--size")) && i + 1 < args.length) {
                ++i;
                switch (args[i]) {
                    case "6": case "6x10": boardConfig = BoardConfig._6x10; break;
                    case "5": case "5x12": boardConfig = BoardConfig._5x12; break;
                    case "4": case "4x15": boardConfig = BoardConfig._4x15; break;
                    case "3": case "3x20": boardConfig = BoardConfig._3x20; break;
                    case "8": case "8x8":  boardConfig = BoardConfig._8x8; break;
                    default:
                        System.err.println("Unknown board size: " + args[i]);
                        return null;
                }
                continue;
            }
            if (args[i].equals("-f") || args[i].equals("--figure")) {
                figure = true;
                continue;
            }
            if (args[i].equals("-q") || args[i].equals("--quiet")) {
                quiet = true; continue;
            }

            break;
        }

        if (i < args.length) {
            System.err.print("Unknown option: ");
            for (; i < args.length; ++i) {
                System.err.print(args[i] + " ");
            }
            System.err.println();
            return null;
        }

        return new Args(boardConfig, figure, quiet);
    }
}

class PrintResultColor implements Solver.Callback {
    private final Piece[] pieces;
    private final BoardConfig boardConfig;

    public PrintResultColor(Piece[] pieces, BoardConfig boardConfig) {
        this.pieces = pieces;
        this.boardConfig = boardConfig;
    }

    public void found(PieceArrange[] arranges) {
        int boardW = boardConfig.width;
        int boardH = boardConfig.height;
        char[] placed = Solver.placedBoard(boardW, boardH, pieces, arranges);
        Set<Character> m = new HashSet<Character>();
        for (int j = 0; j < boardW; ++j) {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < boardH; ++i) {
                char c = placed[i * boardW + j];
                char c2 = ' ';
                if (!m.contains(c)) {
                    c2 = c;
                    m.add(c);
                }
                String s = getColoredChr(c, c2);
                sb.append(s);
            }
            System.out.println(sb.toString());
        }
        System.out.println();
    }

    private static String getColoredChr(char c, char c2) {
        int col;
        switch (c) {
            case 'F':  col = 101; break;
            case 'I':  col =  45; break;
            case 'L':  col = 103; break;
            case 'N':  col = 104; break;
            case 'P':  col = 105; break;
            case 'T':  col = 106; break;
            case 'U':  col =  41; break;
            case 'V':  col =  44; break;
            case 'W':  col =  43; break;
            case 'X':  col = 102; break;
            case 'Y':  col =  42; break;
            case 'Z':  col =  46; break;
            default:   return c2 + " ";
        }
        return String.format("\u001B[%dm%c \u001B[0m", col, c2);
    }
}

class PrintResultFigure implements Solver.Callback {
    private static final char[] GRID_CHARS = new char[] {
        ' ', ' ', ' ', '━',
        ' ', '┏', '┓', '┳',
        ' ', '┗', '┛', '┻',
        '┃', '┣', '┫', '╋',
    };

    private final Piece[] pieces;
    private final BoardConfig boardConfig;

    public PrintResultFigure(Piece[] pieces, BoardConfig boardConfig) {
        this.pieces = pieces;
        this.boardConfig = boardConfig;
    }

    public void found(PieceArrange[] arranges) {
        int w = boardConfig.width;
        int h = boardConfig.height;
        int w2 = w + 1;
        int h2 = h * 2 + 1;

        char[] placed = new char[w2 * h2];
        Arrays.fill(placed, '.');
        for (int i = 0; i < pieces.length; ++i) {
            Piece piece = pieces[i];
            PieceArrange arrange = arranges[i];
            Shape shape = piece.shapes[arrange.shape];
            long bitpat = shape.bitpat;
            while (bitpat != 0) {
                int ofs = BitBoard.trailingZeros(bitpat);
                int xofs = ofs % w;
                int yofs = ofs / w;
                placed[((arrange.y + yofs) * 2 + 1) * w2 + (arrange.x + xofs + 1)] = piece.name;
                bitpat &= bitpat - 1;  // Remove lowest bit.
            }
        }

        boolean hlines[][] = new boolean[h][w + 1];
        for (int y = 0; y < h; ++y) {
            for (int x = 0; x < w + 1; ++x) {
                char c1 = x > 0 ? placed[(y * 2 + 1) * w2 + x] : ' ';
                char c2 = x < w ? placed[(y * 2 + 1) * w2 + (x + 1)] : ' ';
                hlines[y][x] = c1 != c2;
            }
        }

        boolean vlines[][] = new boolean[h + 1][w];
        for (int x = 0; x < w; ++x) {
            for (int y = 0; y < h + 1; ++y) {
                char c1 = y > 0 ? placed[(y * 2 - 1) * w2 + x + 1] : ' ';
                char c2 = y < h ? placed[(y * 2 + 1) * w2 + x + 1] : ' ';
                vlines[y][x] = c1 != c2;
            }
        }

        // Put grid lines.
        for (int x = 0; x < w + 1; ++x) {
            for (int y = 0; y < h + 1; ++y) {
                int i = 0;
                if (y < h && hlines[y    ][x])  i |= 1;
                if (y > 0 && hlines[y - 1][x])  i |= 2;
                if (x < w && vlines[y][x    ])  i |= 4;
                if (x > 0 && vlines[y][x - 1])  i |= 8;
                placed[(y * 2) * w2 + x] = GRID_CHARS[i];
            }
        }
        for (int x = 0; x < w + 1; ++x) {
            for (int y = 0; y < h; ++y) {
                if (hlines[y][x]) {
                    placed[(y * 2 + 1) * w2 + x] = GRID_CHARS[3];
                }
            }
        }

        // Transpose board to reduce lines.
        boolean[] letter_appeared = new boolean[26];
        Arrays.fill(letter_appeared, false);
        for (int x = 0; x < w2; ++x) {
            StringBuilder sb = new StringBuilder();
            for (int y = 0; y < h2; ++y) {
                int i = y * w2 + x;
                char c = placed[i];
                if ('A' <= c && c <= 'Z') {
                    int l = (int)c - (int)'A';
                    if (!letter_appeared[l]) {
                        letter_appeared[l] = true;
                    } else {
                        c = ' ';
                    }
                }
                sb.append(c);
            }
            System.out.println(sb.toString());
        }
        System.out.println();
    }
}

class Args {
    final BoardConfig boardConfig;
    final boolean figure;
    final boolean quiet;

    public Args(BoardConfig boardConfig, boolean figure, boolean quiet) {
        this.boardConfig = boardConfig;
        this.figure = figure;
        this.quiet = quiet;
    }
}
