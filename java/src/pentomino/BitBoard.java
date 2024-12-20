package pentomino;

public class BitBoard {
    private BitBoard() {
    }

    public static long deltaSwap(long x, long mask, int delta) {
        long t = ((x >> delta) ^ x) & mask;
        return t ^ (t << delta) ^ x;
    }

    public static int trailingZeros(long x) {
        return Long.numberOfTrailingZeros(x);
    }

    public static int trailingOnes(long x) {
        return Long.numberOfTrailingZeros(x + 1);
    }
}
