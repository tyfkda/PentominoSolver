import pentomino.component.Piece;
import pentomino.component.Shape;

public class PentominoSolver {
    public static void main(String[] args) throws Exception {
        int boardW = 6;
        int boardH = 10;
        Piece[] pieces = Piece.createShapes(boardW, boardH);
        System.out.println("Piece: #" + pieces.length);
        for (Piece piece : pieces) {
            System.out.println("Piece " + piece.name);
            for (Shape s : piece.shapes) {
                System.out.println("  " + s);
            }
        }
    }
}
