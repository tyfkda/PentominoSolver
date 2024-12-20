import java.util.HashSet;

import pentomino.BoardConfig;
import pentomino.PieceArrange;
import pentomino.Solver;
import pentomino.component.Piece;

public class PentominoSolver {
    public static void main(String[] args_) throws Exception {
        Args args = parseArgs(args_);
        if (args == null)
            System.exit(1);

        int boardW = args.boardConfig.width;
        int boardH = args.boardConfig.height;
        Piece[] pieces = Piece.createShapes(boardW, boardH);

        Solver solver = new Solver(args.boardConfig, pieces);
        Solver.Callback callback = new PrintResultColor(pieces, args.boardConfig);

        solver.setCallback(callback);
        solver.solve();

        System.out.printf("Total: Solution=%d, check=%d\n", solver.solutionCount, solver.checkCount);
    }

    private static Args parseArgs(String[] args) {
        BoardConfig boardConfig = BoardConfig._6x10;
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

        return new Args(boardConfig);
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
        HashSet<Character> m = new HashSet<Character>();
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

class Args {
    final BoardConfig boardConfig;

    public Args(BoardConfig boardConfig) {
        this.boardConfig = boardConfig;
    }
}
