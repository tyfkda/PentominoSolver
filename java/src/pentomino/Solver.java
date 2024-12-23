package pentomino;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.IntStream;

import pentomino.component.Piece;
import pentomino.component.Shape;

public class Solver {
    public interface Callback {
        void found(PieceArrange[] arranges);
    }

    private final Piece[] pieces;
    private final int w;
    private final int h;
    private long bitboard;
    private Callback foundCallback;

    private PieceArrange[] arranges;
    private int arranged = 0;
    public long checkCount = 0;
    public long solutionCount = 0;
    private Set<String> solutionHashes;

    public Solver(BoardConfig boardConfig, Piece[] pieces) {
        this.pieces = pieces;
        this.w = boardConfig.width;
        this.h = boardConfig.height;
        this.bitboard = boardConfig.initialBoard;
        this.foundCallback = null;

        this.arranges = IntStream.range(0, pieces.length)
            .mapToObj(_ -> new PieceArrange())
            .toArray(PieceArrange[]::new);
    }

    public void setCallback(Callback callback) {
        this.foundCallback = callback;
    }

    public void solve() {
        checkCount = 0;
        arranged = 0;
        solutionCount = 0;
        solutionHashes = new HashSet<>();

        int ip = -1;
        for (int i = 0; i < pieces.length; ++i) {
            if (pieces[i].name == 'X') {
                ip = i;
                break;
            }
        }

        // Check X piece first.
        if (ip >= 0)
            solveX(ip);
        else
            solveRecur(0, 0);
    }

    private void solveX(int ip) {
        // Put X piece first in top left quad.
        int is = 0;
        Shape s = pieces[ip].shapes[0];
        int sw = s.w, sh = s.h, ofsx = s.ofsx;
        long saved_bitboard = bitboard;
        for (int y = 0; y <= (h - sh) / 2; ++y) {
            for (int x = ofsx; x <= (w - sw) / 2 + ofsx; ++x) {
                checkCount += 1;
                Shape shape = pieces[ip].shapes[is];
                if (canPutShape(saved_bitboard, w, h, shape, x, y)) {
                    bitboard = putShape(saved_bitboard, w, h, shape, x - shape.ofsx, y);
                    arranges[ip].set(x - shape.ofsx, y, is);
                    arranged |= 1 << ip;
                    solveRecur(0, 0);
                }
            }
        }
    }

    private void solveRecur(int x, int y) {
        checkCount += 1;
        final long saved_bitboard = bitboard;
        int order = arranged;
        for (;;) {
            int ip = Integer.numberOfTrailingZeros(order + 1);  // TrailingOnes.
            if (ip >= pieces.length)
                break;
            order |= 1 << ip;
            for (int is = 0; is < pieces[ip].shapes.length; ++is) {
                Shape shape = pieces[ip].shapes[is];
                if (canPutShape(saved_bitboard, w, h, shape, x, y)) {
                    bitboard = putShape(saved_bitboard, w, h, shape, x - shape.ofsx, y);
                    arranges[ip].set(x - shape.ofsx, y, is);
                    arranged |= 1 << ip;
                    if (arranged != (1 << pieces.length) - 1) {
                        int pos = searchNextPos(bitboard);
                        solveRecur(pos % w, pos / w);
                    } else {
                        // All pieces are placed -> check whether this solution is unique.
                        char[] placed = placedBoard(w, h, pieces, arranges);
                        if (is_unique_solution(placed)) {
                            if (foundCallback != null)
                                foundCallback.found(arranges);
                            add_solution(placed);
                        }
                    }
                }
            }
            arranged &= ~(1 << ip);
        }
    }

    private boolean is_unique_solution(char[] placed) {
        String hash = calcHash(placed);
        return !solutionHashes.contains(hash);
    }

    private void add_solution(char[] placed) {
        solutionCount += 1;

        // Register mirrored.
        char[] mirrored = placed.clone();
        mirrorY(mirrored, w, h);
        solutionHashes.add(calcHash(mirrored));
        mirrorX(mirrored, w, h);
        solutionHashes.add(calcHash(mirrored));
        mirrorY(mirrored, w, h);
        solutionHashes.add(calcHash(mirrored));

        if (w == h) {
            // Register rotated.
            mirrorDiag(mirrored, w);
            solutionHashes.add(calcHash(mirrored));
            mirrorY(mirrored, w, h);
            solutionHashes.add(calcHash(mirrored));
            mirrorX(mirrored, w, h);
            solutionHashes.add(calcHash(mirrored));
            mirrorY(mirrored, w, h);
            solutionHashes.add(calcHash(mirrored));
        }
    }

    private int searchNextPos(long bitboard) {
        int next = BitBoard.trailingOnes(bitboard);
        return next;
    }

    private boolean canPutShape(long board, int w, int h, Shape shape, int x, int y) {
        if (y > h - shape.h) { return false; }
        int ofsx = shape.ofsx;
        return x >= ofsx && x <= w + ofsx - shape.w &&
            (board & (shape.bitpat << (y * w + x - ofsx))) == 0;
    }

    private long putShape(long board, int w, int _h, Shape shape, int x, int y) {
        long bitpat = shape.bitpat << (y * w + x);
        return board | bitpat;
    }


    private static void mirrorX(char[] placed, int w, int h) {
        int m = w >> 1;
        for (int y = 0; y < h; ++y) {
            for (int x = 0; x < m; ++x) {
                int x2 = w - x - 1;
                char t = placed[y * w + x];
                placed[y * w + x] = placed[y * w + x2];
                placed[y * w + x2] = t;
            }
        }
    }

    private static void mirrorY(char[] placed, int w, int h) {
        int m = h >> 1;
        for (int x = 0; x < w; ++x) {
            for (int y = 0; y < m; ++y) {
                int y2 = h - y - 1;
                char t = placed[y * w + x];
                placed[y * w + x] = placed[y2 * w + x];
                placed[y2 * w + x] = t;
            }
        }
    }

    private static void mirrorDiag(char[] placed, int w) {
        assert!(placed.length == w * w);
        for (int y = 0; y < w - 1; ++y) {
            for (int x = y + 1; x < w; ++x) {
                char t = placed[y * w + x];
                placed[y * w + x] = placed[x * w + y];
                placed[x * w + y] = t;
            }
        }
    }

    private static String calcHash(char[] placed) {
        // TODO: Use hash function.
        return new String(placed);
    }

    public static char[] placedBoard(int w, int h, Piece[] pieces, PieceArrange[] arranges) {
        char[] placed = new char[w * h];
        Arrays.fill(placed, ' ');
        for (int i = 0; i < pieces.length; ++i) {
            placePiece(placed, pieces[i], arranges[i], w, h);
        }
        return placed;
    }

    private static void placePiece(char[] placed, Piece piece, PieceArrange arrange, int w, int h) {
        int base_index = arrange.y * w + arrange.x;
        Shape shape = piece.shapes[arrange.shape];
        long bitpat = shape.bitpat;
        while (bitpat != 0) {
            int ofs = BitBoard.trailingZeros(bitpat);
            placed[base_index + ofs] = piece.name;
            bitpat = bitpat & (bitpat - 1);  // Remove lowest bit.
        }
    }
}
