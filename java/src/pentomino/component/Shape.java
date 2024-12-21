package pentomino.component;

import java.util.ArrayList;
import java.util.List;

import pentomino.BitBoard;

public class Shape {
    public final long bitpat;
    public final int w;
    public final int h;
    public final int ofsx;

    Shape(long bitpat, int w, int h, int ofsx) {
        this.bitpat = bitpat;
        this.w = w;
        this.h = h;
        this.ofsx = ofsx;
    }

    public String toString() {
        return "Shape(" + bitpat + ", " + w + ", " + h + ")";
    }

    public boolean equals(Object obj) {
        if (obj == null || !(obj instanceof Shape))
            return false;
        Shape other = (Shape)obj;
        return bitpat == other.bitpat && w == other.w && h == other.h;
    }

    public int hashCode() {
        return (int)((bitpat * (w * h)) % Integer.MAX_VALUE);
    }

    //

    static Shape[] createFromOriginal(OriginalShape os, int boardW, int boardH) {
        Shape baseShape = createBase(os, boardW, boardH);

        List<Shape> shapes = new ArrayList<Shape>();
        for (int i = 0; i < 8; ++i) {
                Shape rotflipped = rotFlip(baseShape, i >= 4, i & 3);
            if (rotflipped.w > boardW || rotflipped.h > boardH)
                continue;

                Shape modified = new Shape(
                toBoardWidth(rotflipped, boardW),
                rotflipped.w,
                rotflipped.h,
                findOfsx(rotflipped.bitpat, rotflipped.w, rotflipped.h)
            );
            if (!shapes.contains(modified))
                shapes.add(modified);
        }
        return shapes.toArray(Shape[]::new);
    }

    static long toBoardWidth(Shape shape, int board_w) {
        // assert!(shape.w <= board_w);
        long bitpat = shape.bitpat;
        if (shape.w < board_w) {
            for (int y = shape.h; --y > 0; ) {
                long mask = (1L << (y * shape.w)) - 1;
                bitpat = (bitpat & mask) | ((bitpat & ~mask) << (board_w - shape.w));
            }
        }
        return bitpat;
    }

    static Shape rotFlip(Shape baseShape, boolean flip, int rot) {
        Shape shape = baseShape;
        if (flip)
            shape = flipY(shape);
        for (int i = 0; i < rot; ++i)
            shape = rot90(shape);
        return shape;
    }

    static Shape flipY(Shape shape) {
        long bitpat = shape.bitpat;
        long line_mask = (1 << shape.w) - 1;
        for (int y = 0; y < shape.h / 2; ++y) {
            bitpat = BitBoard.deltaSwap(bitpat, line_mask << (y * shape.w), (shape.h - 1 - 2 * y) * shape.w);
        }
        return new Shape(bitpat, shape.w, shape.h, 0);
    }

    static Shape rot90(Shape shape) {
        long bitpat = 0;
        for (int y = 0; y < shape.w; ++y) {
            for (int x = 0; x < shape.h; ++x) {
                long b = (shape.bitpat >> ((shape.h - x - 1) * shape.w + y)) & 1;
                bitpat |= b << (y * shape.h + x);
            }
        }
        return new Shape(bitpat, shape.h, shape.w, 0);
    }

    static int findOfsx(long bitpat, int w, int _h) throws AssertionError {
        for (int i = 0; i < w; ++i) {
            if ((bitpat & (1L << i)) != 0)
                return i;
        }
        assert false;
        return -1;
    }

    static Shape createBase(OriginalShape os, int boardW, int boardH) {
        int w = os.lines[0].length();
        int h = os.lines.length;
        long bitpat = 0;
        for (int y = 0; y < h; ++y) {
            String bytes = os.lines[y];
            for (int x = 0; x < w; ++x) {
                long c = bytes.charAt(w - x - 1);
                if (c != 32) {
                    bitpat |= 1 << (y * w + x);
                }
            }
        }
        return new Shape(bitpat, w, h, 0);
    }
}
