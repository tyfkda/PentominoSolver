import pentomino.PieceArrange;
import pentomino.Solver;
import pentomino.component.Piece;

public class PentominoSolver {
    public static void main(String[] args) throws Exception {
        int boardW = 6;
        int boardH = 10;
        Piece[] pieces = Piece.createShapes(boardW, boardH);

        Solver solver = new Solver(boardW, boardH, pieces, 0);
        Solver.Callback callback = new PrintResult(pieces, boardW, boardH);

        solver.setCallback(callback);
        solver.solve();

        System.out.printf("Total: Solution=%d, check=%d\n", solver.solutionCount, solver.checkCount);
    }
}

class PrintResult implements Solver.Callback {
    private final Piece[] pieces;
    private final int boardW;
    private final int boardH;

    public PrintResult(Piece[] pieces, int boardW, int boardH) {
        this.pieces = pieces;
        this.boardW = boardW;
        this.boardH = boardH;
    }

    public void found(PieceArrange[] arranges) {
        char[] placed = Solver.placedBoard(boardW, boardH, pieces, arranges);
        for (int j = 0; j < boardW; ++j) {
            StringBuilder sb = new StringBuilder();
            for (int i = 0; i < boardH; ++i) {
                sb.append(placed[i * boardW + j]);
            }
            System.out.println(sb.toString());
        }
        System.out.println();
    }
}
