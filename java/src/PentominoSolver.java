import java.util.HashSet;

import pentomino.PieceArrange;
import pentomino.Solver;
import pentomino.component.Piece;

public class PentominoSolver {
    public static void main(String[] args) throws Exception {
        int boardW = 6;
        int boardH = 10;
        Piece[] pieces = Piece.createShapes(boardW, boardH);

        Solver solver = new Solver(boardW, boardH, pieces, 0);
        Solver.Callback callback = new PrintResultColor(pieces, boardW, boardH);

        solver.setCallback(callback);
        solver.solve();

        System.out.printf("Total: Solution=%d, check=%d\n", solver.solutionCount, solver.checkCount);
    }
}

class PrintResultColor implements Solver.Callback {
    private final Piece[] pieces;
    private final int boardW;
    private final int boardH;

    public PrintResultColor(Piece[] pieces, int boardW, int boardH) {
        this.pieces = pieces;
        this.boardW = boardW;
        this.boardH = boardH;
    }

    public void found(PieceArrange[] arranges) {
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
