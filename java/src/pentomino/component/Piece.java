package pentomino.component;

import java.util.Arrays;

public class Piece {
    public final char name;
    public final Shape[] shapes;

    Piece(char name, Shape[] shapes) {
        this.name = name;
        this.shapes = shapes;
    }

    //

    public static Piece[] createShapes(int boardW, int boardH) {
        OriginalShape[] oss = new OriginalShape[] {
            new OriginalShape('F', new String[] {
                " @@",
                "@@ ",
                " @ ",
            }),
            new OriginalShape('I', new String[] {
                "@",
                "@",
                "@",
                "@",
                "@",
            }),
            new OriginalShape('L', new String[] {
                "@ ",
                "@ ",
                "@ ",
                "@@",
            }),
            new OriginalShape('N', new String[] {
                " @",
                " @",
                "@@",
                "@ ",
            }),
            new OriginalShape('P', new String[] {
                "@@",
                "@@",
                "@ ",
            }),
            new OriginalShape('T', new String[] {
                "@@@",
                " @ ",
                " @ ",
            }),
            new OriginalShape('U', new String[] {
                "@ @",
                "@@@",
            }),
            new OriginalShape('V', new String[] {
                "@  ",
                "@  ",
                "@@@",
            }),
            new OriginalShape('W', new String[] {
                "@  ",
                "@@ ",
                " @@",
            }),
            new OriginalShape('X', new String[] {
                " @ ",
                "@@@",
                " @ ",
            }),
            new OriginalShape('Y', new String[] {
                " @",
                "@@",
                " @",
                " @",
            }),
            new OriginalShape('Z', new String[] {
                "@@ ",
                " @ ",
                " @@",
            }),
        };

        return Arrays.stream(oss)
            .map(os -> new Piece(os.name, Shape.createFromOriginal(os, boardW, boardH)))
            .toArray(Piece[]::new);
    }
}
